{-# LANGUAGE OverloadedStrings #-}
module Main where

import DB
import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class
import Data.List
import Control.Monad
import Data.Traversable
import System.Directory
import Data.Maybe
import LawSerialization
import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Time.Calendar

import qualified Match
import qualified Player
import qualified Event
import DataStore
import Data.Time.Format
import System.Locale
import qualified Data.Text

main :: IO ()
main = withDatabase $
  do ps   <- liftIO loadPlayerMap
     playerIdMap <- for ps $ \name ->
       addPlayer Player.Player {Player._playerName = Data.Text.pack name}

     let getPlayerId i = fromJust $ Map.lookup i playerIdMap

     dirs <- liftIO $ fmap (\\ [".",".."]) (getDirectoryContents matchDir)
     days <- sort <$> traverse parseDay dirs

     let aux previousEventId day = do
           let e = Event.Event {Event._eventName = "Daily Tournament"
                         ,Event._eventActive = False
                         ,Event._eventDay  = day
                         ,Event._eventPrevious = previousEventId
                         }
           eventId <- addEvent e

           ms <- liftIO $ getMatches day
           for ms $ \(_,m) -> do
             let match = Match.Match { Match._matchWinner = getPlayerId $ winner m
                               , Match._matchLoser  = getPlayerId $ loser m
                               , Match._matchTime   = readTime defaultTimeLocale "%F %X" $ show day ++ " " ++ time m
                               }
             addMatchToEvent match eventId
           return $ Just eventId
     foldM aux Nothing days
     return ()


loadPlayerMap :: IO (Map Int String)
loadPlayerMap = do
  players <- getPlayers
  return (Map.fromList [(playerId player, playerName player) | player <- players])
