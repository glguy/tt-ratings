module Main where

import Control.Lens
import DB
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Law
import LawSerialization
import System.Environment
import Tournament
import TournamentSummaryHtml
import qualified Data.Map as Map

main :: IO ()
main = do
  [dayStr, previousLawsFn, newLawsFn, resultsFn, ratingsFn] <- getArgs

  Just day     <- return (parseDay dayStr)
  playerMap    <- loadPlayerMap
  previousLaws <- loadLaws previousLawsFn
  matches      <- loadMatches day

  let degradedLaws = degradeLaws day previousLaws
      results      = updateLawsForTournament matches degradedLaws

      -- Map.union is left biased, only update laws for those who
      -- played today
      newLaws      = Map.union (fmap (\s -> (day,view summaryFinalLaw s)) results)
                               previousLaws

  namedResults <- nameResults playerMap results
  namedNewLaws <- nameMap playerMap newLaws

  writeFile ratingsFn $ ratingsHtml namedNewLaws
  writeFile resultsFn $ tournamentHtml day namedResults
  writeFile newLawsFn $ serializeLaws newLaws


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
