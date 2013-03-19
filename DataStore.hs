{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataStore where

import Event
import Law
import Match
import Player

import LawSerialization

import Control.Applicative
import Control.Lens
import Control.Monad (liftM)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Time
import Data.Traversable (for)
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Snap.Snaplet.SqliteSimple
import qualified Data.Map as Map

getPlayers :: HasSqlite m => m (Map PlayerId Player)
getPlayers = do xs <- query_ "SELECT playerId, playerName FROM player"
                return $ Map.fromList [(k,v) | Only k :. v <- xs]

getEvents :: HasSqlite m => m (Map EventId Event)
getEvents =
  do xs <- query_ "SELECT eventId, eventDay FROM event"
     return $ Map.fromList [(i,event) | Only i :. event <- xs]

addEvent :: HasSqlite m => Event -> m EventId
addEvent Event{..} =
  do execute "INSERT INTO event (eventDay) VALUES (?)" (Only _eventDay)
     liftM EventId lastInsertRowId

getEventIdByMatchId :: HasSqlite m => MatchId -> m (Maybe EventId)
getEventIdByMatchId matchId = do
  do xs <- query "SELECT eventId FROM match WHERE matchId = ?" (Only matchId)
     return $! case xs of
       []       -> Nothing
       Only x:_ -> Just x

getMatches :: HasSqlite m => m (Map MatchId (Match PlayerId))
getMatches = do
  do xs <- query_ "SELECT matchId, winnerId, loserId, matchTime FROM match"
     return $! Map.fromList [(k,v) | Only k :. v <- xs]

getLatestEventId :: HasSqlite m => m EventId
getLatestEventId = do
  do xs <- query_ "SELECT eventId FROM event ORDER BY eventDay DESC LIMIT 1"
     case xs of
       []       -> fail "No events in system"
       Only x:_ -> return x

getEventIdByDay :: HasSqlite m => Day -> m (Maybe EventId)
getEventIdByDay day =
  do xs <- query "SELECT eventId FROM event WHERE eventDay = ?" (Only day)
     return $! case xs of
       []       -> Nothing
       Only x:_ -> Just x

getEventById :: HasSqlite m => EventId -> m (Maybe Event)
getEventById eventid =
  listToMaybe `liftM` query "SELECT eventDay FROM event WHERE eventId = ?"
                       (Only eventid)

deleteEventById :: HasSqlite m => EventId -> m ()
deleteEventById eventId = do
  execute "DELETE FROM match WHERE eventId = ?" (Only eventId)
  execute "DELETE FROM law   WHERE eventId = ?" (Only eventId)
  execute "DELETE FROM event WHERE eventId = ?" (Only eventId)

addMatchToEvent :: HasSqlite m => Match PlayerId -> EventId -> m MatchId
addMatchToEvent Match{..} eventId =
  do execute "INSERT INTO match (eventId, winnerId, loserId, matchTime)\
             \ VALUES (?,?,?,?)"
       (eventId, _matchWinner, _matchLoser, _matchTime)
     MatchId `liftM` lastInsertRowId

setMatchEventId :: HasSqlite m => MatchId -> EventId -> m ()
setMatchEventId matchId eventId = execute "UPDATE match SET eventId = ? WHERE matchId = ?" (eventId, matchId)

getMatchById :: HasSqlite m => MatchId -> m (Maybe (Match Player))
getMatchById matchid =
  listToMaybe `liftM` query "SELECT w.playerName, l.playerName, matchTime\
              \ FROM match\
              \ JOIN player AS w ON w.playerId = winnerId\
              \ JOIN player AS l ON l.playerId = loserId\
              \ WHERE matchId = ?"
             (Only matchid)

getMatchById' :: HasSqlite m => MatchId -> m (Maybe (Match PlayerId))
getMatchById' matchid =
  listToMaybe `liftM` query "SELECT winnerId, loserId, matchTime\
              \ FROM match\
              \ WHERE matchId = ?"
             (Only matchid)

getMatchTotals :: HasSqlite m => m (Map (PlayerId, PlayerId) Int)
getMatchTotals = do
  xs <- query_ "SELECT winnerId, loserId, COUNT(matchId)\
              \ FROM match\
              \ GROUP BY winnerId, loserId"
  return $ Map.fromList [((w,l),n) | (w,l,n) <- xs]

deleteMatchById :: HasSqlite m => MatchId -> m ()
deleteMatchById matchId =
     execute "DELETE FROM match WHERE matchId = ?" $ Only matchId

addPlayer :: HasSqlite m => Player -> m PlayerId
addPlayer Player{..} =
  do execute "INSERT INTO player (playerName) VALUES (?)" (Only _playerName)
     PlayerId `liftM` lastInsertRowId

getPlayerIdByName :: HasSqlite m => Text -> m (Maybe PlayerId)
getPlayerIdByName name =
  do xs <- query "SELECT playerId FROM player WHERE playerName = ?"
                    (Only name)
     return $! case xs of
       [] -> Nothing
       Only x : _ -> x

getPlayerById :: HasSqlite m => PlayerId -> m (Maybe Player)
getPlayerById playerId =
  listToMaybe `liftM` query "SELECT playerName FROM player WHERE playerId = ?" (Only playerId)

getMatchesForDay :: HasSqlite m => Day -> m [(MatchId, Match Player)]
getMatchesForDay day =
  do xs <- query "SELECT matchId, w.playerName, l.playerName, matchTime\
              \ FROM match\
              \ JOIN player AS w ON w.playerId = winnerId\
              \ JOIN player AS l ON l.playerId = loserId\
              \ WHERE date(matchTime) = ?"
               (Only day)
     return [(x,y) | Only x :. y <- xs]

getMatchesByEventId :: HasSqlite m => EventId -> m (Map MatchId (Match PlayerId))
getMatchesByEventId eventId =
  do xs <- query "SELECT matchId, winnerId, loserId, matchTime\
              \ FROM match\
              \ WHERE eventId = ?"
           (Only eventId)
     return $ Map.fromList [(k,v) | Only k :. v <- xs]

getActivePlayerIds :: HasSqlite m => m [PlayerId]
getActivePlayerIds =
  map fromOnly `liftM` query_ "SELECT playerId FROM player\
                           \ WHERE playerId IN (SELECT winnerId FROM match)\
                           \    OR playerId IN (SELECT loserId FROM match)"

getLawsForEvent :: (Applicative m, HasSqlite m) => Bool -> EventId -> m (Map PlayerId (Day, Law))
getLawsForEvent backOne topEventId = do
  playerIds <- getActivePlayerIds
  fmap (Map.fromList . catMaybes) $
    for playerIds $ \playerId -> do
      xs <-  if backOne
        then query "SELECT eventDay, lawData\
                       \ FROM law\
                       \ NATURAL JOIN event \
                       \ WHERE playerId = ? AND eventId < ?\
                       \ ORDER BY eventDay DESC LIMIT 1"
                             (playerId, topEventId)
        else query "SELECT eventDay, lawData\
                       \ FROM law\
                       \ NATURAL JOIN event \
                       \ WHERE playerId = ? AND eventId <= ?\
                       \ ORDER BY eventDay DESC LIMIT 1"
                             (playerId, topEventId)
      case xs of
        (Only day :. law) : _ -> return (Just (playerId, (day,law)))
        _  -> return Nothing

clearLawsForEvent :: HasSqlite m => EventId -> m ()
clearLawsForEvent eventId = execute "DELETE FROM law WHERE eventId = ?" (Only eventId)

addLaw :: HasSqlite m => PlayerId -> EventId -> Law -> m ()
addLaw playerId eventId law =
  execute "INSERT INTO law (playerId, eventId, mean, stddev, lawData) VALUES (?,?,?,?,?)"
    (playerId, eventId, lawMean law, lawStddev law, serializeLaw law)

getLawsForPlayer :: HasSqlite m => PlayerId -> m (Map EventId (Event, Law))
getLawsForPlayer playerId = do
  xs <- query "SELECT eventId, eventDay, lawData\
               \ FROM law\
               \ NATURAL JOIN event \
               \ WHERE playerId = ?" (Only playerId)
  return $ Map.fromList [(k,(event,law)) | Only k :. event :. law <- xs]

lastInsertRowId :: HasSqlite m => m Int64
lastInsertRowId = withSqlite Sqlite.lastInsertRowId

--
-- ID Types
--

newtype EventId  = EventId  Int64 deriving (Read, Show, Eq, Ord)
newtype PlayerId = PlayerId Int64 deriving (Read, Show, Eq, Ord)
newtype MatchId  = MatchId  Int64 deriving (Read, Show, Eq, Ord)

makeWrapped ''EventId
makeWrapped ''PlayerId
makeWrapped ''MatchId

instance ToField EventId  where toField (EventId  i) = toField i
instance ToField PlayerId where toField (PlayerId i) = toField i
instance ToField MatchId  where toField (MatchId  i) = toField i

instance FromField EventId  where fromField = fmap EventId . fromField
instance FromField PlayerId where fromField = fmap PlayerId . fromField
instance FromField MatchId  where fromField = fmap MatchId . fromField

instance FromRow PlayerId   where fromRow = PlayerId <$> field

instance FromRow Law        where
  fromRow = do dat <- field
               case deserializeLaw dat of
                 Nothing -> fail "bad law data"
                 Just law -> return law

instance FromRow Player where
  fromRow = do _playerName <- field
               return Player {..}

instance FromRow p => FromRow (Match p) where
  fromRow = do _matchWinner <- fromRow
               _matchLoser  <- fromRow
               _matchTime   <- field
               return Match {..}

instance FromRow Event where
  fromRow = do _eventDay      <- field
               return Event {..}
