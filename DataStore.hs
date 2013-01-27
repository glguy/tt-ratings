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
import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Maybe
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time
import Data.Traversable (for)
import Database.SQLite.Simple (lastInsertRowId)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Snap.Snaplet.SqliteSimple
import qualified Data.Map as Map

getPlayers :: HasSqlite m => m (Map PlayerId Player)
getPlayers = do xs <- query_ "SELECT playerId, playerName FROM player"
                return $ Map.fromList [(k,v) | Only k :. v <- xs]

getEvents :: HasSqlite m => m (Map EventId (Event EventId))
getEvents =
  do xs <- query_ "SELECT eventId, eventName, eventDay, eventActive, previousEventId FROM event"
     return $ Map.fromList [(i,event) | Only i :. event <- xs]

addEvent :: HasSqlite m => Event EventId -> m EventId
addEvent Event{..} =
  do execute "INSERT INTO event (eventName, eventDay, eventActive, previousEventId) VALUES (?,?,?,?)"
       (_eventName, _eventDay, _eventActive, _eventPrevious)
     liftM EventId lastInsertRowId'

getCurrentEventId :: HasSqlite m => m (Maybe EventId)
getCurrentEventId =
  do xs <- query_ "SELECT eventId FROM event WHERE eventActive = 1 ORDER BY eventDay DESC LIMIT 1"
     return $! case xs of
       []       -> Nothing
       Only x:_ -> Just x

getLatestEventId :: HasSqlite m => m (Maybe EventId)
getLatestEventId =
  do xs <- query_ "SELECT eventId FROM event ORDER BY eventDay DESC, eventDay DESC LIMIT 1"
     return $! case xs of
       []       -> Nothing
       Only x:_ -> Just x

getEventById :: HasSqlite m => EventId -> m (Maybe (Event EventId))
getEventById eventid =
  listToMaybe `liftM` query "SELECT eventName, eventDay, eventActive, previousEventId\
                           \ FROM event WHERE eventId = ?"
                       (Only eventid)

deleteEventById :: HasSqlite m => EventId -> m ()
deleteEventById eventId = do
  execute "DELETE FROM match WHERE eventId = ?" (Only eventId)
  execute "DELETE FROM law   WHERE eventId = ?" (Only eventId)
  execute "DELETE FROM event WHERE eventId = ?" (Only eventId)

setEventActive :: HasSqlite m => EventId -> Bool -> m ()
setEventActive eventId active =
  execute "UPDATE event SET eventActive = ? WHERE eventId = ?"
           (active, eventId)

addMatchToEvent :: HasSqlite m => Match PlayerId -> EventId -> m MatchId
addMatchToEvent Match{..} eventId =
  do [Only active] <- query "SELECT eventActive FROM event WHERE eventId = ?" (Only eventId)
     unless active $ fail "Attempting to add match to an inactive event"
     execute "INSERT INTO match (eventId, winnerId, loserId, matchTime) VALUES (?,?,?,?)"
       (eventId, _matchWinner, _matchLoser, _matchTime)
     MatchId `liftM` lastInsertRowId'

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

deleteMatchById :: HasSqlite m => MatchId -> m ()
deleteMatchById matchId =
     execute "DELETE FROM match WHERE matchId = ?" $ Only matchId

addPlayer :: HasSqlite m => Player -> m PlayerId
addPlayer Player{..} =
  do execute "INSERT INTO player (playerName) VALUES (?)" (Only _playerName)
     PlayerId `liftM` lastInsertRowId'

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

getLawsForEvent :: (Applicative m, HasSqlite m) => EventId -> m (Map PlayerId (Day, Law))
getLawsForEvent topEventId = do
  playerIds <- getActivePlayerIds

  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  let today = localDay now

  fmap Map.fromList $ for playerIds $ \playerId -> do
    law <- fromMaybeT (today, defaultLaw) $ search playerId =<< parentEvent topEventId
    return (playerId, law)
  where

  parentEvent :: HasSqlite m => EventId -> MaybeT m EventId
  parentEvent e = do
    Only (Just x) : _ <- query "SELECT previousEventId FROM event WHERE eventId = ?" (Only e)
    -- Pattern match failure handled by MaybeT
    return x

  search :: HasSqlite m => PlayerId -> EventId -> MaybeT m (Day, Law)
  search playerId eventId =
    do laws <- query "SELECT eventDay,lawData\
                             \ FROM law\
                             \ JOIN event USING (eventId)\
                             \ WHERE playerId = ? AND eventId = ?"
                             (playerId, eventId)
       case laws of
         [] -> do e <- parentEvent eventId
                  search playerId e
         (Only day :. law):_ -> return (day,law)

clearLawsForEvent :: HasSqlite m => EventId -> m ()
clearLawsForEvent eventId = execute "DELETE FROM law WHERE eventId = ?" (Only eventId)

addLaw :: HasSqlite m => PlayerId -> EventId -> Law -> m ()
addLaw playerId eventId law =
  execute "INSERT INTO law (playerId, eventId, mean, stddev, lawData) VALUES (?,?,?,?,?)"
    (playerId, eventId, lawMean law, lawStddev law, serializeLaw law)

getLawsForPlayer :: HasSqlite m => PlayerId -> m (Map EventId (Event EventId, Law))
getLawsForPlayer playerId = do
  xs <- query "SELECT eventId, eventName, eventDay, eventActive, previousEventId, lawData\
               \ FROM law\
               \ NATURAL JOIN event \
               \ WHERE playerId = ?" (Only playerId)
  return $ Map.fromList [(k,(event,law)) | Only k :. event :. law <- xs]

lastInsertRowId' :: HasSqlite m => m Int64
lastInsertRowId' = withSqlite lastInsertRowId

fromMaybeT :: Functor m => x -> MaybeT m x -> m x
fromMaybeT x m = fromMaybe x <$> runMaybeT m

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

instance FromField p => FromRow (Event p) where
  fromRow = do _eventName     <- field
               _eventDay      <- field
               _eventActive   <- field
               _eventPrevious <- field
               return Event {..}

instance HasSqlite m => HasSqlite (MaybeT m) where
  getSqliteState = lift getSqliteState
