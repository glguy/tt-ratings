{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataStore where

import Event
import Law
import Match
import Player

import LawSerialization
import MaybeT

import Control.Applicative
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad (join, liftM, ap)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Traversable (for)
import Data.Maybe (fromMaybe, listToMaybe)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Int (Int64)
import Data.Time
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

dbName = "pingpong.db"

withDatabase :: DatabaseT IO a -> IO a
withDatabase (DatabaseT f) = withConnection dbName f

getPlayerList :: DatabaseM m => m [Player]
getPlayerList =
     map fromOnly <$> query_' "SELECT playerName FROM player"

getPlayerMap :: DatabaseM m => m (Map PlayerId Player)
getPlayerMap =
     Map.fromList <$> query_' "SELECT playerId, playerName FROM player"

{-
getEvents :: IO [(EventId, Event)]
getEvents = withConnection dbName $ \db ->
  do xs <- query db "SELECT playerId, playerName FROM player" ()
     return [(PlayerId i, Player { .. }) | (i,_playerName) <- xs]
-}

addEvent :: DatabaseM m => Event EventId -> m EventId
addEvent Event{..} =
  do execute' "INSERT INTO event (eventName, eventDay, eventActive, previousEventId) VALUES (?,?,?,?)"
       (_eventName, _eventDay, _eventActive, _eventPrevious)
     EventId <$> lastInsertRowId'

getCurrentEventId :: DatabaseM m => m (Maybe EventId)
getCurrentEventId =
  do xs <- query_' "SELECT eventId FROM event WHERE eventActive = 1 ORDER BY eventDay DESC LIMIT 1"
     return $! case xs of
       []       -> Nothing
       Only x:_ -> Just x

getEventById :: DatabaseM m => EventId -> m (Maybe (Event EventId))
getEventById eventid =
  listToMaybe <$> query' "SELECT eventName, eventDay, eventActive, previousEventId\
                           \ FROM event WHERE eventId = ?"
                       (Only eventid)

addMatchToEvent :: DatabaseM m => Match PlayerId -> EventId -> m MatchId
addMatchToEvent Match{..} event =
  do execute' "INSERT INTO match (eventId, winnerId, loserId, matchTime) VALUES (?,?,?,?)"
       (event, _matchWinner, _matchLoser, _matchTime)
     MatchId <$> lastInsertRowId'

getMatchById :: DatabaseM m => MatchId -> m (Maybe (Match Player))
getMatchById matchid =
  do xs <- query' "SELECT w.playerName, l.playerName, matchTime\
              \ FROM match\
              \ JOIN player AS w ON w.playerId = winnerId\
              \ JOIN player AS l ON l.playerId = loserId\
              \ WHERE matchId = ?"
             (Only matchid)
     return $! case xs of
       []  -> Nothing
       x:_ -> Just x

deleteMatchById :: DatabaseM m => MatchId -> m ()
deleteMatchById matchId =
     execute' "DELETE FROM match WHERE matchId = ?" $ Only matchId

addPlayer :: DatabaseM m => Player -> m PlayerId
addPlayer Player{..} =
  do execute' "INSERT INTO player (playerName) VALUES (?)" (Only _playerName)
     PlayerId <$> lastInsertRowId'

getPlayerIdByName :: DatabaseM m => Text -> m (Maybe PlayerId)
getPlayerIdByName name =
  do xs <- query' "SELECT playerId FROM player WHERE playerName = ?"
                    (Only name)
     return $! case xs of
       [] -> Nothing
       Only x : _ -> x

getMatchesForDay :: DatabaseM m => Day -> m [(MatchId, Match Player)]
getMatchesForDay day =
  do xs <- query' "SELECT matchId, w.playerName, l.playerName, matchTime\
              \ FROM match\
              \ JOIN player AS w ON w.playerId = winnerId\
              \ JOIN player AS l ON l.playerId = loserId\
              \ WHERE date(matchTime) = ?"
               (Only day)
     return [(x,y) | Only x :. y <- xs]

getMatchesByEventId :: DatabaseM m => EventId -> m [Match PlayerId]
getMatchesByEventId eventId =
  query' "SELECT winnerId, loserId, matchTime\
              \ FROM match\
              \ WHERE eventId = ?"
           (Only eventId)

getActivePlayerIds :: DatabaseM m => m [PlayerId]
getActivePlayerIds =
  map fromOnly <$> query_' "SELECT playerId FROM player\
                           \ WHERE playerId IN (SELECT winnerId FROM match)\
                           \    OR playerId IN (SELECT loserId FROM match)"

getLawsForEvent :: DatabaseM m => EventId -> m (Map PlayerId (Day, Law))
getLawsForEvent eventId = do
  playerIds <- getActivePlayerIds

  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  let today = localDay now

  fmap Map.fromList $ for playerIds $ \playerId -> do
    law <- fromMaybeT (today, defaultLaw) $ search playerId =<< parentEvent eventId
    return (playerId, law)
  where

  parentEvent :: DatabaseM m => EventId -> MaybeT m EventId
  parentEvent e = do
    Only (Just x) : _ <- query' "SELECT previousEventId FROM event WHERE eventId = ?" (Only e)
    -- Pattern match failure handled by MaybeT
    return x

  search :: DatabaseM m => PlayerId -> EventId -> MaybeT m (Day, Law)
  search playerId eventId =
    do laws <- query' "SELECT eventDay,lawData\
                             \ FROM law\
                             \ JOIN event using (eventId)\
                             \ WHERE playerId = ? AND eventId = ?"
                             (playerId, eventId)
       case laws of
         [] -> do e <- parentEvent eventId
                  search playerId e
         (day,x):_ -> case deserializeRow1 x of
                       Nothing -> error "bad law data"
                       Just law -> return (day,law)

clearLawsForEvent :: DatabaseM m => EventId -> m ()
clearLawsForEvent eventId = execute' "DELETE FROM law WHERE eventId = ?" (Only eventId)

addLaw :: DatabaseM m => PlayerId -> EventId -> Law -> m ()
addLaw playerId eventId law =
  execute' "INSERT INTO law (playerId, eventId, mean, stddev, lawData) VALUES (?,?,?,?,?)"
    (playerId, eventId, lawMean law, lawStddev law, serializeRow1 law)


newtype DatabaseT m a = DatabaseT (Connection -> m a)
runDatabaseT con (DatabaseT f) = f con

instance MonadIO m => MonadIO (DatabaseT m) where
  liftIO m = DatabaseT $ \_ -> liftIO m

instance Functor m => Functor (DatabaseT m) where
  fmap f (DatabaseT g) = DatabaseT $ fmap f . g

instance Applicative m => Applicative (DatabaseT m) where
  DatabaseT f <*> DatabaseT g = DatabaseT $ \con -> f con <*> g con
  pure x = DatabaseT $ \_ -> pure x

instance Monad m => Monad (DatabaseT m) where
  m >>= f  = DatabaseT $ \con -> runDatabaseT con m >>= runDatabaseT con . f
  fail e   = DatabaseT $ \_ -> fail e
  return x = DatabaseT $ \_ -> return x

instance MonadTrans DatabaseT where
  lift m = DatabaseT $ \_ -> m

class (MonadIO m, Monad m, Applicative m) => DatabaseM m where
  connection :: m Connection

instance (MonadIO m, Monad m, Applicative m) => DatabaseM (DatabaseT m) where
  connection = DatabaseT $ \con -> return con

instance DatabaseM m => DatabaseM (MaybeT m) where
  connection = lift connection

query' :: (DatabaseM m, ToRow q, FromRow r) => Query -> q -> m [r]
query' q args = do
  con <- connection
  liftIO $ query con q args

query_' :: (DatabaseM m, FromRow r) => Query -> m [r]
query_' q = do
  con <- connection
  liftIO $ query_ con q

execute' :: (DatabaseM m, ToRow q) => Query -> q -> m ()
execute' q args = do
  con <- connection
  liftIO $ execute con q args

lastInsertRowId' :: DatabaseM m => m Int64
lastInsertRowId' = liftIO . lastInsertRowId =<< connection

--
-- ID Types
--

newtype EventId  = EventId  Int64 deriving (Read, Show, Eq, Ord)
newtype PlayerId = PlayerId Int64 deriving (Read, Show, Eq, Ord)
newtype MatchId  = MatchId  Int64 deriving (Read, Show, Eq, Ord)

instance ToField EventId  where toField (EventId  i) = toField i
instance ToField PlayerId where toField (PlayerId i) = toField i
instance ToField MatchId  where toField (MatchId  i) = toField i

instance FromField EventId  where fromField = fmap EventId . fromField
instance FromField PlayerId where fromField = fmap PlayerId . fromField
instance FromField MatchId  where fromField = fmap MatchId . fromField

instance FromField Player where
  fromField field = do _playerName <- fromField field
                       return Player {..}

instance FromField p => FromRow (Match p) where
  fromRow = do _matchWinner <- field
               _matchLoser  <- field
               _matchTime   <- field
               return Match {..}

instance FromField p => FromRow (Event p) where
  fromRow = do _eventName     <- field
               _eventDay      <- field
               _eventActive   <- field
               _eventPrevious <- field
               return Event {..}
