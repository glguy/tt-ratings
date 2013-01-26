{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import GraphData
import LawSerialization
import System.Environment
import Tournament
import TournamentSummaryHtml
import qualified Data.Map as Map

import Law
import Match
import Event
import DataStore

data Config = Config
  { today :: Day
  , currentEventId :: EventId
  , resultsFn, ratingsFn :: FilePath
  }

main :: IO ()
main = do
  config <- getConfig

  playerMap    <- getPlayerMap
  previousLaws <- getLawsForEvent $ currentEventId config
  matches      <- getMatchesByEventId $ currentEventId config

  let degradedDayLaws = fmap (\(day,law) -> (day, degradeLaw (today config) day law)) previousLaws
      degradedLaws    = fmap snd degradedDayLaws
      results         = evaluateTournament matches degradedLaws

      -- Map.union is left biased, only update laws for those who
      -- played today
      selectLawFromResults s = (today config,view summaryFinalLaw s)
      newLaws      = Map.union (fmap selectLawFromResults results) previousLaws

      -- We display degraded laws but we don't serialize them
      -- This way only one degrade operation occurs between events per law
      todaysLaws   = Map.union (fmap selectLawFromResults results) degradedDayLaws

  namedResults <- nameResults playerMap results
  namedNewLaws <- nameMap playerMap newLaws
  namedTodaysLaws <- nameMap playerMap todaysLaws

  writeFile (ratingsFn config) $ ratingsHtml (today config) namedTodaysLaws
  writeFile (resultsFn config) $ tournamentHtml (today config) namedResults
  -- writeFile "graph.js" $ generateFlotData $ fmap snd namedTodaysLaws

getConfig :: IO Config
getConfig = do
  args <- getArgs
  case args of
    [dayStr, currentEventStr, resultsFn, ratingsFn] ->
      do today <- parseDay dayStr
         let currentEventId = EventId $ read currentEventStr
         return Config {..}
    _ -> fail "usage: tt-ratings DAY OLD_LAWS NEW_LAWS RESULTS RATINGS"

nameMap :: (Monad m, Ord k, Ord k') => Map k k' -> Map k v -> m (Map k' v)
nameMap names
  = maybe (fail "nameMap: missing names") return
  . traverseKeys (flip Map.lookup names)

nameResults :: (Ord k, Ord v) => Map k v -> Map k (PlayerSummary k) -> IO (Map v (PlayerSummary v))
nameResults players
  = maybe (fail "Unknown player number") return
  . tournamentNameTraversal (flip Map.lookup players)
