{-# LANGUAGE RecordWildCards, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Exception ()
import Control.Lens
import Control.Monad.IO.Class
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Time
import Data.Foldable (for_)
import Data.Traversable (for)
import System.Locale
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (shamlet, Html)
import Data.Text.Read
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc

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
import Snap.Snaplet.SqliteSimple

data App = App
  { _db :: Snaplet Sqlite
  }

makeClassy ''App
-- class HasApp t where
--  app :: Lens' t App
--  db  :: Lens' t (Snaplet Sqlite)

instance HasApp app => HasSqlite (Handler b app) where getSqliteState = with db get

appInit :: SnapletInit App App
appInit = makeSnaplet "tt-ratings" "Ping pong ratings application" Nothing $ do
  d <- embedSnaplet "db" db sqliteInit
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

matchPostHandler :: Handler App App ()
matchPostHandler = do
  winner <- getParam' "winner"
  loser  <- getParam' "loser"
  Just winnerId <- getPlayerIdByName winner
  Just loserId  <- getPlayerIdByName loser
  _matchId <- saveMatch winnerId loserId
  liftIO $ putStrLn $ "Saving " ++ show (winner,loser)
  redirect "/"

matchopPostHandler :: Handler App App ()
matchopPostHandler = do
  action  <- getParam' "action"
  matchId <- MatchId <$> getNumParam "matchId"
  case action of
      "delete"       -> deleteMatchById matchId
      "duplicate"  ->
        do mbMatch <- getMatchById' matchId
           for_ mbMatch $ \match ->
             saveMatch (view matchWinner match) (view matchLoser match)
      _ -> return ()
  redirect "/"

exportPlayersHandler :: Handler App App ()
exportPlayersHandler = do
  ms <- getPlayers
  let xs = [ (op PlayerId k, view playerName v) | (k,v) <- Map.toList ms]
  modifyResponse $ setContentType "text/plain; charset=utf-8"
  writeText $ Text.pack $ show xs

exportMatchesHandler :: Handler App App ()
exportMatchesHandler = do
  ms <- exportMatches
  modifyResponse $ setContentType "text/plain; charset=utf-8"
  writeText $ Text.pack $ show ms

eventsGetHandler :: Handler App App ()
eventsGetHandler = do
  events <- getEvents
  sendHtml $ eventsPage events

eventsPostHandler :: Handler App App ()
eventsPostHandler = do
  action <- getParam' "action"
  eventId <- EventId <$> getNumParam "eventId"
  case action of
      "Open"   -> setEventActive eventId True
      "Close"  -> setEventActive eventId False
      "Delete" -> deleteEventById eventId
      _        -> fail "Unknown operation"
  redirect "/events"

playerHandler :: Handler App App ()
playerHandler = do
  playerId <- PlayerId <$> getNumParam "playerId"
  html     <- playerPage playerId
  sendHtml html

playersHandler :: Handler App App ()
playersHandler = do
  Just eventId <- getLatestEventId
  players  <- getPlayers
  today    <- localDay <$> getLocalTime
  eventMap <- getLawsForEvent eventId
  let Just eventMap' = ifor eventMap $ \i (a,b) ->
                             do player <- Map.lookup i players
                                return (player,a,b)
  sendHtml $ playersHtml today eventMap'

curvesHandler :: Handler App App ()
curvesHandler = do
  Just eventId <- getLatestEventId
  players      <- getPlayers
  eventMap     <- getLawsForEvent eventId
  let Just eventMap' =
         for (Map.toList eventMap) $ \(i,(_,law)) ->
           do player <- Map.lookup i players
              return (player,law)

  modifyResponse $ setContentType "application/javascript; charset=utf8"
  writeText $ Text.pack $ generateFlotData $ Map.fromList eventMap'


defaultHandler :: Handler App App ()
defaultHandler = sendHtml =<< mainPage

mainPage :: Handler App App Html
mainPage =
  do t  <- getLocalTime
     tz <- liftIO getCurrentTimeZone
     mb <- getCurrentEventId
     ps <- getPlayers
     ms <- case mb of
             Nothing            -> return Map.empty
             Just eventId       -> getMatchesByEventId eventId
     let Just namedMatches = (each . traverse) (flip Map.lookup ps) ms
     return $ thePage (Map.elems ps) $ formatMatches tz (localDay t) namedMatches

saveMatch :: (Functor m, HasSqlite m, MonadIO m) => PlayerId -> PlayerId -> m MatchId
saveMatch _matchWinner _matchLoser = do
  _matchTime <- liftIO getCurrentTime
  eventId <- do mb <- getCurrentEventId
                case mb of
                  Just eventId -> return eventId
                  Nothing      -> fail "No event is currently open"
  addMatchToEvent Match{..} eventId

sendHtml :: MonadSnap m => Html -> m ()
sendHtml html = do
  modifyResponse $ setContentType "text/html; charset=utf-8"
  writeBuilder $ renderHtmlBuilder html

getLocalTime :: MonadIO m => m LocalTime
getLocalTime = zonedTimeToLocalTime `liftM` liftIO getZonedTime

getParam' :: MonadSnap m => Text -> m Text
getParam' name = do
  mb <- getParam $ Enc.encodeUtf8 name
  case mb of
    Nothing -> fail ("Missing parameter: " ++ show name)
    Just x  -> return $ Enc.decodeUtf8 x

getNumParam :: (Integral a, MonadSnap m) => Text -> m a
getNumParam name = do
   p <- getParam' name
   case decimal p of
     Right (n,rest)
       | Text.null rest -> return n
       | otherwise      -> fail "bad number"
     Left err -> fail err

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

thePage :: [Player] -> Html -> Html
thePage ps table = [shamlet|
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
