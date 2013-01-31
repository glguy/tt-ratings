{-# LANGUAGE RecordWildCards, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
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
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc

import DataStore
import Event
import Law (lawElems)
import Match
import Output.Common
import Output.Events
import Output.ExportMatches
import Output.Formatting
import Output.Player
import Output.Players
import Output.TournamentSummary
import Player
import TournamentCompiler

import Snap
import Snap.Util.FileServe
import Snap.Snaplet.SqliteSimple

data App = App
  { _db :: Snaplet Sqlite
  , _tournamentReports :: MVar (Map EventId Html)
  }

data RunState = Running | Rerun | Idle

makeClassy ''App
-- class HasApp t where
--  app :: Lens' t App
--  db  :: Lens' t (Snaplet Sqlite)

instance HasApp app => HasSqlite (Handler b app) where getSqliteState = with db get

appInit :: SnapletInit App App
appInit = makeSnaplet "tt-ratings" "Ping pong ratings application" Nothing $ do
  _db <- embedSnaplet "db" db sqliteInit
  _tournamentReports <- liftIO $ newMVar Map.empty

  addRoutes
     [ ("match",                method POST matchPostHandler)
     , ("matchop",              method POST matchopPostHandler)
     , ("exportplayers",        exportPlayersHandler)
     , ("exportmatches",        exportMatchesHandler)
     , ("events",               eventsGetHandler)
     , ("event/latest",         method GET latestEventHandler)
     , ("event/:eventId",       method GET eventHandler)
     , ("player/:playerId",     playerHandler)
     , ("players",              playersHandler)
     , ("curves.js",            curvesHandler)
     , ("static",               serveDirectory "static")
     , ("favicon.ico",          serveFile "static/ping-pong.png")
     , ("",                     defaultHandler)
     ]

  return App { .. }

main :: IO ()
main = serveSnaplet defaultConfig appInit

matchPostHandler :: Handler App App ()
matchPostHandler = do
  winner <- getParam' "winner"
  loser  <- getParam' "loser"

  let m `check` e = maybe failure return =<< m
        where
        failure = do
            sendHtml   =<< mainPage (Just e) winner loser
            finishWith =<< getResponse

  winnerId <- getPlayerIdByName winner `check` "Unknown winner"
  loserId  <- getPlayerIdByName loser  `check` "Unknown loser"
  saveMatch winnerId loserId
  redirect "/"

matchopPostHandler :: Handler App App ()
matchopPostHandler = do
  action  <- getParam' "action"
  matchId <- MatchId <$> getNumParam "matchId"
  case action of
      "delete" ->
        do Just eventId <- getEventIdByMatchId matchId
           Just event   <- getEventById eventId
           today        <- localDay <$> getLocalTime
           unless (view eventDay event == today)
                  (fail "This match can not be deleted")
           deleteMatchById matchId
           _ <- rerunEvent True eventId
           return ()
      "duplicate"  ->
        do mbMatch <- getMatchById' matchId
           for_ mbMatch $ \match ->
             saveMatch (view matchWinner match) (view matchLoser match)
      _ -> return ()
  redirect "/"

rerunEvent :: Bool -> EventId -> Handler App App Html
rerunEvent changed eventId = do
  (event,report) <- generateTournamentSummary changed eventId
  let html = tournamentHtml event report
  var <- use tournamentReports
  liftIO $ modifyMVar_ var $ \cache -> return $ cache & at eventId ?~ html
  return html

exportPlayersHandler :: Handler App App ()
exportPlayersHandler = do
  ms <- getPlayers
  let xs = [ (op PlayerId k, view playerName v) | (k,v) <- Map.toList ms]

  callbackWrapper $ sendJson xs

callbackWrapper :: Handler App App a -> Handler App App a
callbackWrapper m = do
  mb <- getParam "callback"
  traverse_ (\bs -> writeBS bs >> writeBS "(") mb
  x <- m
  traverse_ (\_  -> writeBS ")") mb
  return x

exportMatchesHandler :: Handler App App ()
exportMatchesHandler = callbackWrapper . sendJson =<< exportMatches

eventsGetHandler :: Handler App App ()
eventsGetHandler = do
  events <- getEvents
  sendHtml $ eventsPage events

latestEventHandler :: Handler App App ()
latestEventHandler = do
  eventId <- getLatestEventId
  redirect $ Enc.encodeUtf8 $ mkEventUrl eventId

eventHandler :: Handler App App ()
eventHandler = do
  eventId <- EventId <$> getNumParam "eventId"
  var     <- use tournamentReports
  cache   <- liftIO $ readMVar var
  html    <- case view (at eventId) cache of
    Just html   -> return html
    Nothing     -> rerunEvent False eventId
  sendHtml html

playerHandler :: Handler App App ()
playerHandler = do
  playerId <- PlayerId <$> getNumParam "playerId"
  html     <- playerPage playerId
  sendHtml html

playersHandler :: Handler App App ()
playersHandler = do
  eventId <- getLatestEventId
  players  <- getPlayers
  today    <- localDay <$> getLocalTime
  eventMap <- getLawsForEvent False eventId
  let Just eventMap' = ifor eventMap $ \i (a,b) ->
                             do player <- Map.lookup i players
                                return (player,a,b)
  sendHtml $ playersHtml today eventMap'

curvesHandler :: Handler App App ()
curvesHandler = do
  eventId <- getLatestEventId
  players      <- getPlayers
  eventMap     <- getLawsForEvent False eventId
  let Just curveData =
         for (Map.toList eventMap) $ \(i,(_,law)) ->
           do player <- Map.lookup i players
              return (view playerName player, lawElems law)

  callbackWrapper $ sendJson curveData


defaultHandler :: Handler App App ()
defaultHandler = sendHtml =<< mainPage Nothing "" ""

mainPage ::
  Maybe Text {- ^ error message -} ->
  Text {- ^ initial winner text -} ->
  Text {- ^ initial loser  text -} ->
  Handler App App Html
mainPage err w l =
  do now <- liftIO getZonedTime
     let today = localDay (zonedTimeToLocalTime now)
         tz    = zonedTimeZone now
     mb <- getEventIdByDay today
     ps <- getPlayers
     ms <- case mb of
             Nothing            -> return Map.empty
             Just eventId       -> getMatchesByEventId eventId
     let Just namedMatches = (each . traverse) (flip Map.lookup ps) ms
     return $ thePage err w l (Map.elems ps)
            $ formatMatches tz today namedMatches

saveMatch :: PlayerId -> PlayerId -> Handler App App ()
saveMatch _matchWinner _matchLoser = do
  _matchTime <- liftIO getCurrentTime
  today <- localDay . zonedTimeToLocalTime
       <$> liftIO (utcToLocalZonedTime _matchTime)

  eventId <- do
    mb <- getEventIdByDay today
    case mb of
      Just eventId      -> return eventId
      Nothing           -> addEvent Event { _eventDay = today }

  _ <- addMatchToEvent Match{..} eventId
  _ <- rerunEvent True eventId
  return ()

sendJson :: (Aeson.ToJSON a, MonadSnap m) => a -> m ()
sendJson x = do
  modifyResponse $ setContentType "application/json; charset=utf-8"
  writeLBS $ Aeson.encode x

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
formatMatches tz d xs
  | Map.null xs = return ()
  | otherwise = [shamlet|
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

thePage :: Maybe Text -> Text -> Text -> [Player] -> Html -> Html
thePage err w l ps table = [shamlet|
<html lang=en>
  <head>
    ^{metaTags}
    <title>Ping Pong Results
    <link rel=stylesheet type=text/css href=static/common.css>
    <link rel=stylesheet type=text/css href=static/style.css>
  <body>
    ^{navigationLinks}
    <div .entry>
      <form action="/match" method=POST>
        <label for=winner>Winner:
        <input autocomplete=off list=players name=winner #winner value=#{w}>
        <label for=loser>Loser:
        <input autocomplete=off list=players name=loser  #loser value=#{l}>
        <datalist #players>
          $forall p <- ps
            <option value=#{view playerName p}>
        <input type=submit #submit value=Record>
    $maybe errMsg <- err
      <div #errorMessage>#{errMsg}
    ^{table}
|]
