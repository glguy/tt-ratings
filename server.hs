{-# LANGUAGE RecordWildCards, PatternGuards, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import Control.Applicative
import Control.Exception ()
import Control.Lens
import Control.Monad.IO.Class
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Monoid
import Data.Ord (comparing)
import Data.Time
import Data.Foldable (for_)
import Data.Traversable (for)
import Network.URL
import System.Locale
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (shamlet, Html)
import Text.Read(readMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc
import Codec.Binary.UTF8.Generic (toString)

import DataStore
import Match
import Output.Common
import Output.Events
import Output.ExportMatches
import Output.Formatting
import Output.GraphLawCurves (generateFlotData)
import Output.Player
import Output.Players
import Player

import Snap
import Snap.Util.FileServe
import Snap.Http.Server

import Snap.Snaplet.SqliteSimple
data App = App
  { _db :: Snaplet Sqlite
  }

makeLenses ''App

instance HasSqlite (Handler b App) where
   getSqliteState = with db get

appInit :: SnapletInit App App
appInit = makeSnaplet "tt-ratings" "Ping pong ratings application" Nothing $ do
  d <- nestSnaplet "db" db sqliteInit
  addRoutes
     [ ("match", method POST matchPostHandler)
     , ("matchop", method POST matchopPostHandler)
     , ("exportplayers", exportPlayersHandler)
     , ("exportmatches", exportMatchesHandler)
     , ("events", method GET eventsGetHandler)
     , ("events", method POST eventsPostHandler)
     , ("player", playerHandler)
     , ("players", playersHandler)
     , ("curves.js", curvesHandler)
     ]
  wrapSite (<|> dir "static" (serveDirectory "static"))
  wrapSite (<|> defaultHandler)

  return $ App { _db = d }

main :: IO ()
main = serveSnaplet defaultConfig appInit

matchPostHandler = do
  winner <- toString <$> getParam' "winner"
  loser  <- toString <$> getParam' "loser"
  Just winnerId <- getPlayerIdByName $ Text.pack winner
  Just loserId  <- getPlayerIdByName $ Text.pack loser
  _matchId <- saveMatch winnerId loserId
  liftIO $ putStrLn $ "Saving " ++ show (winner,loser)
  redirect "/"

matchopPostHandler = do
  action  <- toString <$> getParam' "action"
  matchId <- fmap MatchId . liftIO . readIO . toString =<< getParam' "matchId"
  case action of
      "delete"       -> deleteMatchById matchId
      "duplicate"  ->
        do mbMatch <- getMatchById' matchId
           for_ mbMatch $ \match ->
             saveMatch (view matchWinner match) (view matchLoser match)
      _ -> return ()
  redirect "/"

exportPlayersHandler = do
  ms <- getPlayers
  let xs = [ (op PlayerId k, view playerName v) | (k,v) <- Map.toList ms]
  writeText $ Text.pack $ show xs

exportMatchesHandler = do
  ms <- exportMatches
  writeText $ Text.pack $ show ms

eventsGetHandler = do
  events <- getEvents
  writeBuilder $ renderHtmlBuilder $ eventsPage events

eventsPostHandler = do
  action <- toString <$> getParam' "action"
  eventId <- EventId . read . toString <$> getParam' "eventId"
  case action of
      "Open"   -> setEventActive eventId True
      "Close"  -> setEventActive eventId False
      "Delete" -> deleteEventById eventId
      _        -> fail "Unknown operation"
  redirect "/events"

playerHandler = do
  playerId <- fmap PlayerId . liftIO . readIO . toString =<< getParam' "playerId"
  html <- playerPage playerId
  writeBuilder $ renderHtmlBuilder html

playersHandler = do
  Just eventId <- getLatestEventId
  players <- getPlayers
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  dat <- getLawsForEvent eventId
  let Just dat1 = ifor dat $ \i (a,b) ->
                             do player <- Map.lookup i players
                                return (player,a,b)
  writeBuilder $ renderHtmlBuilder $ playersHtml today dat1

curvesHandler = do
           Just eventId <- getLatestEventId
           players <- getPlayers
           dat <- getLawsForEvent eventId
           let Just dat1 = for (Map.toList dat) $ \(i,(_,law)) ->
                             do player <- Map.lookup i players
                                return (player,law)
           writeText $ Text.pack $ generateFlotData $ Map.fromList dat1


defaultHandler = writeBuilder . renderHtmlBuilder =<< mainPage

mainPage =
  do t   <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
     tz  <- liftIO $ getCurrentTimeZone
     mbeventId  <- getCurrentEventId
     ps  <- getPlayers
     ms <- case mbeventId of
             Nothing -> return Map.empty
             Just eventId -> getMatchesByEventId eventId
     let Just namedMatches = (each . traverse) (flip Map.lookup ps) ms
     liftIO $ thePage (Map.elems ps) $ formatMatches tz (localDay t) namedMatches

saveMatch :: (Functor m, HasSqlite m, MonadIO m) => PlayerId -> PlayerId -> m MatchId
saveMatch _matchWinner _matchLoser = do
  _matchTime <- liftIO getCurrentTime
  eventId <- do mb <- getCurrentEventId
                case mb of
                  Just eventId -> return eventId
                  Nothing      -> fail "No event is currently open"
  addMatchToEvent Match{..} eventId

getParam' name = do
  mb <- getParam name
  case mb of
    Nothing -> fail ("Missing parameter: " ++ show name)
    Just x  -> return x

--------------------------------------------------------------------------------

formatMatch :: TimeZone -> Int -> MatchId -> Match Player -> Html
formatMatch tz i (MatchId mid) match = [shamlet|
  <tr :odd i:.alt>
    <td>#{t}
    <td>#{w}
    <td>#{l}
    <td>
      <form .deleteform action="/matchop" method=post>
        <input type=hidden name=matchId value=#{show mid}>
        <input .deletebutton type=submit name=action value=duplicate>
        <input .deletebutton type=submit name=action value=delete>
|]
  where
  t = view (matchTime   . to (formatTime defaultTimeLocale "%X" . utcToLocalTime tz)) match
  w = view (matchWinner . playerName) match
  l = view (matchLoser  . playerName) match

formatMatches :: TimeZone -> Day -> Map MatchId (Match Player) -> Html
formatMatches tz d xs = [shamlet|
<h2>Matches for #{formatLongDay d}
  <table>
    <tr>
      <th>Time
      <th>Winner
      <th>Loser
      <th>Actions
    $forall (i,(fn,m)) <- itoList $ sortBy (flip byTime) $ Map.toList xs
      ^{formatMatch tz i fn m}
|]
  where
  byTime = comparing (view matchTime . snd)

thePage :: [Player] -> Html -> IO Html
thePage ps table =
     return [shamlet|
<html lang=en>
  <head>
    ^{metaTags}
    <title>Ping Pong Results
    <link rel=stylesheet type=text/css href=static/common.css>
    <link rel=stylesheet type=text/css href=static/style.css>
  <body>
    <div .entry>
      <form action="/match" method=POST>
        <label for=winner>Winner:
        <input autocomplete=off list=players name=winner #winner>
        <label for=loser>Loser:
        <input autocomplete=off list=players name=loser  #loser>
        <datalist #players>
          $forall p <- ps
            <option value=#{view playerName p}>
        <input type=submit #submit value=Record>
    ^{navigationLinks}
    ^{table}
|]
