{-# LANGUAGE RecordWildCards, PatternGuards, QuasiQuotes #-}
module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Time
import Data.Foldable (for_)
import Data.Traversable (for)
import Network.HTTP.Server
import Network.HTTP.Server.HtmlForm
import Network.HTTP.Server.Logger
import Network.URL
import System.Locale
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamlet, Html)
import Text.Read(readMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Codec.Binary.UTF8.String (encodeString)

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

main :: IO ()
main = serverWith
  Config
    { srvLog = quietLogger
    , srvHost = "0.0.0.0"
    , srvPort = 8000
    }
    $ \_ URL { .. } request ->
  case splitOn "/" url_path of
    ["match"]
      | POST <- rqMethod request
      , Just form <- fromRequest request
      , Just winner <- lookupString form "winner"
      , Just loser  <- lookupString form "loser" ->
         withDatabase $ do
           Just winnerId <- getPlayerIdByName $ Text.pack winner
           Just loserId  <- getPlayerIdByName $ Text.pack loser
           _matchId <- saveMatch winnerId loserId
           liftIO $ putStrLn $ "Saving " ++ show (winner,loser)
           return $ redir "/"

    ["matchop"]
      | POST <- rqMethod request
      , Just form <- fromRequest request
      , Just action  <- lookupString form "action"
      , Just matchId <- MatchId <$> lookupRead form "matchId" ->
             withDatabase $ do
               case action of
                 "delete"       -> deleteMatchById matchId
                 "duplicate"  ->
                    do mbMatch <- getMatchById' matchId
                       for_ mbMatch $ \match ->
                         saveMatch (view matchWinner match) (view matchLoser match)
                 _ -> return ()
               return $ redir "/"

    ["exportplayers"] ->
      do ms <- withDatabase getPlayers
         return $ ok $ show
           [ (op PlayerId k, view playerName v) | (k,v) <- Map.toList ms]

    ["exportmatches"] ->
      do ms <- withDatabase exportMatches
         return $ ok $ show ms

    ["events"]
       | GET <- rqMethod request ->
              do events <- withDatabase getEvents
                 return $ ok $ eventsPage events

       | POST <- rqMethod request
       , Just form    <- fromRequest request
       , Just action  <- lookupString form "action"
       , Just eventId <- EventId <$> lookupRead   form "eventId" ->
           withDatabase $ do
             case action of
               "Open"   -> setEventActive eventId True
               "Close"  -> setEventActive eventId False
               "Delete" -> deleteEventById eventId
               _        -> fail "Unknown operation"

             return $ redir "/events"

    ["player"]
       | GET <- rqMethod request
       , Just playerId <- fmap PlayerId . readMaybe =<< lookup "playerId" url_params ->
         withDatabase $ do
           html <- playerPage playerId
           return $ ok $ renderHtml html

    ["players"] ->
        withDatabase $ do
           Just eventId <- getLatestEventId
           players <- getPlayers
           today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
           dat <- getLawsForEvent eventId
           let Just dat1 = ifor dat $ \i (a,b) ->
                             do player <- Map.lookup i players
                                return (player,a,b)
           return $ ok $ renderHtml $ playersHtml today dat1


    ["curves.js"] ->
       withDatabase $ do
           Just eventId <- getLatestEventId
           players <- getPlayers
           dat <- getLawsForEvent eventId
           let Just dat1 = for (Map.toList dat) $ \(i,(_,law)) ->
                             do player <- Map.lookup i players
                                return (player,law)
           return $ ok $ generateFlotData $ Map.fromList dat1

    ["static","common.css" ] -> ok <$> readFile "static/common.css"
    ["static","style.css"  ] -> ok <$> readFile "static/style.css"
    ["static","results.css"] -> ok <$> readFile "static/results.css"
    ["static","ratings.css"] -> ok <$> readFile "static/ratings.css"
    ["static","jquery.flot.js"] -> ok <$> readFile "static/jquery.flot.js"
    ["static","graph.html"] -> ok <$> readFile "static/graph.html"

    _ -> ok . renderHtml <$> mainPage
  `catch` \(SomeException e) -> return (bad (show e))

  where
  ok body = Response { rspCode   = (2,0,0)
                     , rspReason = "OK"
                     , rspHeaders = hdrs
                     , rspBody   = encodeString body }

  hdrs = [ mkHeader HdrConnection "close"]

  redir str = Response { rspCode = (3,0,2)
                  , rspReason = "Found"
                  , rspHeaders = hdrs ++ [ mkHeader HdrLocation str ]
                  , rspBody = "OK"
                  }

  bad e = Response { rspCode = (5,0,0)
                   , rspReason = "It didn't work"
                   , rspHeaders = hdrs
                   , rspBody = e
                   }

mainPage :: IO Html
mainPage = withDatabase $
  do t   <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
     tz  <- liftIO $ getCurrentTimeZone
     mbeventId  <- getCurrentEventId
     ps  <- getPlayers
     ms <- case mbeventId of
             Nothing -> return Map.empty
             Just eventId -> getMatchesByEventId eventId
     let Just namedMatches = (each . traverse) (flip Map.lookup ps) ms
     liftIO $ thePage (Map.elems ps) $ formatMatches tz (localDay t) namedMatches

saveMatch :: DatabaseM m => PlayerId -> PlayerId -> m MatchId
saveMatch _matchWinner _matchLoser = do
  _matchTime <- liftIO getCurrentTime
  eventId <- do mb <- getCurrentEventId
                case mb of
                  Just eventId -> return eventId
                  Nothing      -> fail "No event is currently open"
  addMatchToEvent Match{..} eventId

--------------------------------------------------------------------------------

formatMatch :: TimeZone -> Int -> MatchId -> Match Player -> Html
formatMatch tz i (MatchId mid) match = [shamlet|
  <tr :odd i:.alt>
    <td>#{t}
    <td>#{w}
    <td>#{l}
    <td>
      <form .deleteform action="/matchop" method=post enctype="multipart/form-data">
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
      <form action="/match" method=POST enctype="multipart/form-data">
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
