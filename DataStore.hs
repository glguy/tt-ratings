{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataStore where

import Event
import Law
import Match
import Player

import LawSerialization

import Control.Applicative
import Control.Monad (join)
import Data.Traversable (for)
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Int (Int64)
import Data.Time
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

dbName = "pingpong.db"


getPlayerList :: IO [Player]
getPlayerList = withConnection dbName $ \db ->
     map fromOnly <$> query_ db "SELECT playerName FROM player"

getPlayerMap :: IO (Map PlayerId Player)
getPlayerMap = withConnection dbName $ \db ->
     Map.fromList <$> query_ db "SELECT playerId, playerName FROM player"

{-
getEvents :: IO [(EventId, Event)]
getEvents = withConnection dbName $ \db ->
  do xs <- query db "SELECT playerId, playerName FROM player" ()
     return [(PlayerId i, Player { .. }) | (i,_playerName) <- xs]
-}

addEvent :: Event EventId -> IO EventId
addEvent Event{..} = withConnection dbName $ \db ->
  do execute db "INSERT INTO event (eventName, eventDay, eventActive, previousEventId) VALUES (?,?,?,?)"
       (_eventName, _eventDay, _eventActive, _eventPrevious)
     EventId <$> lastInsertRowId db

getCurrentEventId :: IO (Maybe EventId)
getCurrentEventId = withConnection dbName $ \db ->
  do xs <- query_ db "SELECT eventId FROM event WHERE eventActive = 1 ORDER BY eventDay DESC LIMIT 1"
     return $! case xs of
       []       -> Nothing
       Only x:_ -> Just x

getEventById :: EventId -> IO (Maybe (Event EventId))
getEventById eventid = withConnection dbName $ \db ->
  listToMaybe <$> query db "SELECT eventName, eventDay, eventActive, previousEventId\
                           \ FROM event WHERE eventId = ?"
                       (Only eventid)

addMatchToEvent :: Match PlayerId -> EventId -> IO MatchId
addMatchToEvent Match{..} event = withConnection dbName $ \db ->
  do execute db "INSERT INTO match (eventId, winnerId, loserId, matchTime) VALUES (?,?,?,?)"
       (event, _matchWinner, _matchLoser, _matchTime)
     MatchId <$> lastInsertRowId db

getMatchById :: MatchId -> IO (Maybe (Match Player))
getMatchById matchid = withConnection dbName $ \db ->
  do xs <- query db "SELECT w.playerName, l.playerName, matchTime\
              \ FROM match\
              \ JOIN player AS w ON w.playerId = winnerId\
              \ JOIN player AS l ON l.playerId = loserId\
              \ WHERE matchId = ?"
             (Only matchid)
     return $! case xs of
       []  -> Nothing
       x:_ -> Just x

deleteMatchById :: MatchId -> IO ()
deleteMatchById matchId = withConnection dbName $ \db ->
     execute db "DELETE FROM match WHERE matchId = ?" $ Only matchId

addPlayer :: Player -> IO PlayerId
addPlayer Player{..} = withConnection dbName $ \db ->
  do execute db "INSERT INTO player (playerName) VALUES (?)" (Only _playerName)
     PlayerId <$> lastInsertRowId db

getPlayerIdByName :: Text -> IO (Maybe PlayerId)
getPlayerIdByName name = withConnection dbName $ \db ->
  do [Only i] <- query db "SELECT playerId FROM player WHERE playerName = ?"
                    (Only name)
     return i

getMatchesForDay :: Day -> IO [(MatchId, Match Player)]
getMatchesForDay day = withConnection dbName $ \db ->
  do xs <- query db "SELECT matchId, w.playerName, l.playerName, matchTime\
              \ FROM match\
              \ JOIN player AS w ON w.playerId = winnerId\
              \ JOIN player AS l ON l.playerId = loserId\
              \ WHERE date(matchTime) = ?"
               (Only day)
     return [(x,y) | Only x :. y <- xs]

getMatchesByEventId :: EventId -> IO [Match PlayerId]
getMatchesByEventId eventId = withConnection dbName $ \db ->
  query db "SELECT winnerId, loserId, matchTime\
              \ FROM match\
              \ WHERE eventId = ?"
           (Only eventId)

getLawsForEvent :: EventId -> IO (Map PlayerId (Day, Law))
getLawsForEvent eventId = withConnection dbName $ \db -> do
  playerIds <- map fromOnly <$> query_ db "SELECT playerId FROM player"
  fmap Map.fromList $ for playerIds $ \playerId -> do
    law <- search db playerId eventId
    return (playerId, law)
  where
  parentEvent db e = do
    xs <- query db "SELECT previousEventId FROM event WHERE eventId = ?" (Only e)
    return $! case xs of
      Only (Just x) : _ -> Just x
      _ -> Nothing
  search db playerId eventId =
    do laws <- query db "SELECT eventDay,lawData\
                             \ FROM law\
                             \ JOIN event using (eventId)\
                             \ WHERE playerId = ? AND eventId = ?"
                             (playerId, eventId)
       case laws of
         [] -> do e <- parentEvent db eventId
                  case e of
                    Just e -> search db playerId e
                    Nothing -> do now <- zonedTimeToLocalTime <$> getZonedTime
                                  return (localDay now,defaultLaw)
         (day,x):_ -> case deserializeRow1 x of
                       Nothing -> fail "bad law data"
                       Just law -> return (day,law)

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
