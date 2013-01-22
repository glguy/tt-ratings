{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import DB
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Law
import GraphData
import LawSerialization
import System.Environment
import Tournament
import TournamentSummaryHtml
import qualified Data.Map as Map

data Config = Config
  { today :: Day
  , previousLawsFn, newLawsFn, resultsFn, ratingsFn :: FilePath
  }

main :: IO ()
main = do
  config <- getConfig

  playerMap    <- loadPlayerMap
  previousLaws <- loadLaws $ previousLawsFn config
  matches      <- loadMatches $ today config

  let degradedDayLaws = fmap (\(day,law) -> (day, degradeLaw (today config) day law)) previousLaws
      degradedLaws    = fmap snd degradedDayLaws
      results      = updateLawsForTournament matches degradedLaws

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
  writeFile (newLawsFn config) $ serializeLaws newLaws
  writeFile "graph.js" $ generateFlotData $ fmap snd namedTodaysLaws

getConfig = do
  args <- getArgs
  case args of
    [dayStr, previousLawsFn, newLawsFn, resultsFn, ratingsFn] ->
      do today <- parseDay dayStr
         return Config {..}
    _ -> fail "usage: tt-ratings DAY OLD_LAWS NEW_LAWS RESULTS RATINGS"

loadPlayerMap = do
  players <- getPlayers
  return (Map.fromList [(playerId player, playerName player) | player <- players])

loadMatches :: Day -> IO [Match Int]
loadMatches day = fmap (fmap snd) (getMatches day)

loadLaws :: FilePath -> IO (Map Int (Day, Law))
loadLaws fn = do
  txt <- readFile fn
  Just laws <- return (deserializeLaws txt)
  return laws

nameMap names
  = maybe (fail "nameMap: missing names") return
  . traverseKeys (flip Map.lookup names)

nameResults :: Map Int String -> Map Int (PlayerSummary Int) -> IO (Map String (PlayerSummary String))
nameResults players
  = maybe (fail "Unknown player number") return
  . tournamentNameTraversal (flip Map.lookup players)
