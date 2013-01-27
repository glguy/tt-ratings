{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Map (Map)
import System.Environment
import Tournament
import Output.GraphLawCurves
import Output.TournamentSummaryHtml
import qualified Data.Map as Map

import Event
import DataStore

data Config = Config
  { currentEventId :: EventId
  , resultsFn, ratingsFn :: FilePath
  }

main :: IO ()
main = withDatabase $ do
  config <- liftIO getConfig

  playerMap    <- getPlayerMap
  event        <- do mb <- getEventById $ currentEventId config
                     case mb of
                       Nothing -> fail "No such event"
                       Just x -> return x
  previousLaws <- getLawsForEvent $ currentEventId config
  matches      <- fmap Map.elems $ getMatchesByEventId $ currentEventId config

  let today = view eventDay event

  let degradedDayLaws = fmap (\(day,law) -> (day, degradeLaw today day law)) previousLaws
      degradedLaws    = fmap snd degradedDayLaws
      results         = evaluateTournament matches degradedLaws

      -- Map.union is left biased, only update laws for those who
      -- played today
      selectLawFromResults s = (today,view summaryFinalLaw s)
      newLawsFromEvent = fmap selectLawFromResults results

      -- We display degraded laws but we don't serialize them
      -- This way only one degrade operation occurs between events per law
      todaysLaws   = Map.union newLawsFromEvent degradedDayLaws

  namedResults <- nameResults playerMap results
  namedTodaysLaws <- nameMap playerMap todaysLaws

  clearLawsForEvent $ currentEventId config
  ifor_ newLawsFromEvent $ \playerId (_day,law) -> addLaw playerId (currentEventId config) law

  liftIO $ writeFile (ratingsFn config) $ ratingsHtml today namedTodaysLaws
  liftIO $ writeFile (resultsFn config) $ tournamentHtml today namedResults
  liftIO $ writeFile "graph.js" $ generateFlotData $ fmap snd namedTodaysLaws

getConfig :: IO Config
getConfig = do
  args <- getArgs
  case args of
    [currentEventStr, resultsFn, ratingsFn] ->
      do let currentEventId = EventId $ read currentEventStr
         return Config {..}
    _ -> fail "usage: tt-ratings EVENTID RESULTS RATINGS"

nameMap :: (Monad m, Ord k, Ord k') => Map k k' -> Map k v -> m (Map k' v)
nameMap names
  = maybe (fail "nameMap: missing names") return
  . traverseKeys (flip Map.lookup names)

nameResults :: Monad m => (Ord k, Ord v) => Map k v -> Map k (PlayerSummary k) -> m (Map v (PlayerSummary v))
nameResults players
  = maybe (fail "Unknown player number") return
  . tournamentNameTraversal (flip Map.lookup players)
