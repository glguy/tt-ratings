{-# LANGUAGE RecordWildCards, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import Control.Lens ( use, view, (?~), makeClassy, op, ifor, At(at) )
import Control.Monad (liftM, replicateM_, when, unless)
import Control.Monad.State (get)
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Read ( decimal )
import Data.Time ( Day, LocalTime(localDay), UTCTime, ZonedTime(zonedTimeToLocalTime), getZonedTime, utcToLocalZonedTime, getCurrentTime )
import NewTTRS.Law (lawElems)
import NewTTRS.Match ( matchLoser, matchWinner, Match(_matchTime, Match, _matchWinner, _matchLoser) )
import NewTTRS.Tournament (degradeLaw)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (Html)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc

import DataStore ( MatchId(..), PlayerId(..), EventId(..), getPlayers, getEvents, addEvent, getEventIdByMatchId, getLatestEventId, getEventIdByDay, getEventById, addMatchToEvent, getMatchById', getMatchTotals, deleteMatchById, getPlayerIdByName, getLawsForEvent )
import Event ( Event(_eventDay, Event), eventDay )
import MatchEntry (matchEntryPage)
import Output.Common ( mkEventUrl )
import Output.Events ( eventsPage )
import Output.ExportMatches ( exportMatches )
import Output.Player ( playerPage )
import Output.Players ( playersHtml )
import Output.TournamentSummary ( tournamentHtml )
import Output.Totals ( totalsHtml )
import Player ( playerName )
import TournamentCompiler ( generateTournamentSummary )

import Snap
import Snap.Util.FileServe ( serveDirectory, serveFile )
import Snap.Snaplet.SqliteSimple ( sqliteInit, HasSqlite(..), Sqlite )

data App = App
  { _db :: Snaplet Sqlite
  , _tournamentReports :: IORef (Map EventId Html)
  }

makeClassy ''App
-- class HasApp t where
--  app :: Lens' t App
--  db  :: Lens' t (Snaplet Sqlite)

instance HasSqlite (Handler b App) where getSqliteState = with db get

appInit :: SnapletInit App App
appInit = makeSnaplet "tt-ratings" "Ping pong ratings application" Nothing $ do
  _db <- embedSnaplet "db" db sqliteInit
  _tournamentReports <- liftIO (newIORef Map.empty)

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
     , ("totals",               totalsHandler)
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
  winsText <- getParam' "wins"
  loser  <- getParam' "loser"
  lossesText <- getParam' "losses"
  create <- ("True"==) <$> getParam' "create"

  let m `check` e = maybe (failure e) return =<< m
      failure txt = do
            sendHtml   =<< matchEntryPage (Just txt) winner winsText loser lossesText True
            finishWith =<< getResponse

  let parseOutcome txt =
          case decimal txt of
            Right (x,xs) | Text.null xs -> return x
            _                           -> failure "Invalid outcomes"

  wins   <- parseOutcome winsText
  losses <- parseOutcome lossesText

  when (   wins < 0
        || losses < 0
        || wins > 9                 -- sanity
        || losses > 9               -- sanity
        || losses == 0 && wins == 0 -- no-op
       )
    (failure "Invalid outcomes")

  winnerId <- getPlayerIdByName create winner `check` "Unknown winner"
  loserId  <- getPlayerIdByName create loser  `check` "Unknown loser"
  saveMatch winnerId wins loserId losses
  redirect "/"

-- The user clicked one of the buttons next to a match
-- on the match entry screen. Figure out which one and
-- dispatch.
matchopPostHandler :: Handler App App ()
matchopPostHandler = do
  action  <- getParam' "action"
  matchId <- MatchId <$> getNumParam "matchId"
  case action of
    "delete"    -> deleteMatchHandler    matchId
    "copy"      -> duplicateMatchHandler matchId
    "swapped"   -> upsetMatchHandler     matchId
    _           -> fail "Unknown action"

duplicateMatchHandler :: MatchId -> Handler App App ()
duplicateMatchHandler matchId = do
  match <- getMatchById' matchId `onNothing` "Unknown match"
  saveMatch (view matchWinner match) 1 (view matchLoser match) 0
  redirect "/"

upsetMatchHandler :: MatchId -> Handler App App ()
upsetMatchHandler matchId = do
  match <- getMatchById' matchId `onNothing` "Unknown match"
  saveMatch (view matchWinner match) 0 (view matchLoser match) 1
  redirect "/"

deleteMatchHandler :: MatchId -> Handler App App ()
deleteMatchHandler matchId = do
  eventId <- getEventIdByMatchId matchId `onNothing` "Unknown match"
  event   <- getEventById eventId        `onNothing` "Unknown event"
  today   <- localDay <$> getLocalTime
  unless (view eventDay event == today) (fail "This match can not be deleted")
  deleteMatchById matchId
  _ <- rerunEvent True eventId
  redirect "/"

rerunEvent :: Bool -> EventId -> Handler App App Html
rerunEvent changed eventId = do
  -- This might update the database if changed is True
  (event,report) <- generateTournamentSummary changed eventId
  allLaws        <- getLawsForEvent True eventId
  players        <- getPlayers
  let html       =  tournamentHtml players event report allLaws
  var            <- use tournamentReports
  liftIO (modifyIORef var (at eventId ?~ html))
  return html

exportPlayersHandler :: Handler App App ()
exportPlayersHandler = do
  ms <- getPlayers
  sendJson [ (op PlayerId k, view playerName v) | (k,v) <- Map.toList ms]

exportMatchesHandler :: Handler App App ()
exportMatchesHandler = sendJson =<< exportMatches

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
  cache   <- liftIO (readIORef var)
  html    <- case view (at eventId) cache of
               Just html -> return html
               Nothing   -> rerunEvent False eventId
  sendHtml html

playerHandler :: Handler App App ()
playerHandler = do
  playerId <- PlayerId <$> getNumParam "playerId"
  html     <- playerPage playerId
  sendHtml html

playersHandler :: Handler App App ()
playersHandler = do
  eventId       <- getLatestEventId
  players       <- getPlayers
  today         <- localDay <$> getLocalTime
  eventMap      <- getLawsForEvent False eventId
  eventMap'     <- maybe (fail "Unknown player id in event") return
                 $ ifor eventMap $ \i (day,law) ->
                     do player <- Map.lookup i players
                        return (player,day,degradeLaw today day law)
  sendHtml $ playersHtml today eventMap'

totalsHandler :: Handler App App ()
totalsHandler = do
  players       <- getPlayers
  totals        <- getMatchTotals
  eventId       <- getLatestEventId
  eventMap      <- getLawsForEvent False eventId
  sendHtml $ totalsHtml players totals eventMap

curvesHandler :: Handler App App ()
curvesHandler = do
  chosenEventId <- getOptParam "eventid"
  eventId       <- case chosenEventId of
                     Just x -> case decimal x of
                                 Right (n,xs) | Text.null xs -> return (EventId n)
                                 _                           -> fail "Bad event id"
                     Nothing -> getLatestEventId
  players       <- getPlayers
  eventMap      <- getLawsForEvent False eventId
  curveData     <- maybe (fail "Unknown player id in event") (return . toList)
                 $ ifor eventMap $ \i (_,law) ->
                     do player <- Map.lookup i players
                        return (view playerName player, lawElems law)
  sendJson curveData


defaultHandler :: Handler App App ()
defaultHandler = sendHtml =<< matchEntryPage Nothing "" "1" "" "0" False

timeToEventDay :: UTCTime -> IO Day
timeToEventDay time = fmap (localDay . zonedTimeToLocalTime) (utcToLocalZonedTime time)

saveMatch :: PlayerId -> Int -> PlayerId -> Int -> Handler App App ()
saveMatch winner wins loser losses = do
  _matchTime <- liftIO getCurrentTime

  eventId <- do
    _eventDay <- liftIO (timeToEventDay _matchTime)
    mb <- getEventIdByDay _eventDay
    case mb of
      Just eventId      -> return eventId
      Nothing           -> addEvent Event { .. }

  replicateM_ wins
     (addMatchToEvent Match{ _matchWinner = winner, _matchLoser = loser, ..} eventId)
  replicateM_ losses
     (addMatchToEvent Match{ _matchWinner = loser, _matchLoser = winner, ..} eventId)
  _ <- rerunEvent True eventId
  return ()

sendJson :: (Aeson.ToJSON a, MonadSnap m) => a -> m ()
sendJson x = do
  mb <- getParam "callback"
  case mb of
    Nothing ->
      do modifyResponse (setContentType "application/json; charset=utf-8")
         writeLBS (Aeson.encode x)
    Just callback ->
      do modifyResponse (setContentType "application/javascript; charset=utf-8")
         writeBS callback
         writeBS "("
         writeLBS (Aeson.encode x)
         writeBS ")"

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
    Nothing -> error ("Missing parameter: " ++ show name)
    Just x  -> return $ Enc.decodeUtf8 x

getOptParam :: MonadSnap m => Text -> m (Maybe Text)
getOptParam name = fmap (fmap Enc.decodeUtf8) (getParam (Enc.encodeUtf8 name))

getNumParam :: (Integral a, MonadSnap m) => Text -> m a
getNumParam name = do
   p <- getParam' name
   case decimal p of
     Right (n,rest)
       | Text.null rest -> return n
       | otherwise      -> error "bad number"
     Left err -> error err


--------------------------------------------------------------------------------



onNothing :: Monad m => m (Maybe a) -> String -> m a
onNothing m str = maybe (error str) return =<< m
