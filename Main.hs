module Main where

import Law
import GraphData
import Tournament
import TournamentSummaryHtml
import LawSerialization
import System.Environment
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Calendar
import DB

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
      newLaws      = Map.union (fmap (\(law,_) -> (day,law)) results)
                               previousLaws

  namedResults <- nameResults playerMap results

  writeFile ratingsFn $ ratingsHtml (nameMap playerMap newLaws)
  writeFile resultsFn $ tournamentHtml (nameMap playerMap degradedLaws) day namedResults
  writeFile newLawsFn $ serializeLaws newLaws
  writeFile "graph.js" $ generateSimpleFlotData $ nameMap playerMap $ fmap snd newLaws


loadPlayerMap = do
  players <- getPlayers
  return (Map.fromList [(playerId player, playerName player) | player <- players])

loadMatches day = do
  matches <- getMatches day
  return [Tournament.Match { matchWinner = winner m, matchLoser = loser m }
         | (_,m) <- matches]

loadLaws :: FilePath -> IO (Map Int (Day, Law))
loadLaws fn = do
  txt <- readFile fn
  Just laws <- return (deserializeLaws txt)
  return laws

nameMap names
  = Map.fromList
  . map (\(k,v) -> let Just name = Map.lookup k names in (name, v))
  . Map.toList

nameResults :: Map Int String -> TournamentSummary Int -> IO (TournamentSummary String)
nameResults players results = do
  case tournamentNameTraversal (`Map.lookup` players) results of
    Nothing -> fail ("Unknown player number")
    Just x -> return x
