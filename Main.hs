{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import Control.Concurrent.MVar (newMVar)
import System.Environment
import Tournament
import Output.TournamentSummaryHtml
import qualified Data.Map as Map
import Text.Blaze.Html.Renderer.String (renderHtml)

import Database.SQLite.Simple (withConnection)
import Snap.Snaplet.SqliteSimple (Sqlite(Sqlite))

import Event
import DataStore

data Config = Config
  { currentEventId :: EventId
  , resultsFn :: FilePath
  }

runDb m = withConnection dbName $ \db ->
  do v <- newMVar db
     runReaderT m $ Sqlite v

main :: IO ()
main = runDb $ do
  config <- liftIO getConfig

  playerMap    <- getPlayers
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

  namedResults <- nameResults playerMap results

  clearLawsForEvent $ currentEventId config
  ifor_ newLawsFromEvent $ \playerId (_day,law) -> addLaw playerId (currentEventId config) law

  liftIO $ writeFile (resultsFn config) $ renderHtml $ tournamentHtml today namedResults

getConfig :: IO Config
getConfig = do
  args <- getArgs
  case args of
    [currentEventStr, resultsFn] ->
      do let currentEventId = EventId $ read currentEventStr
         return Config {..}
    _ -> fail "usage: tt-ratings EVENTID RESULTS"

nameMap :: (Monad m, Ord k, Ord k') => Map k k' -> Map k v -> m (Map k' v)
nameMap names
  = maybe (fail "nameMap: missing names") return
  . traverseKeys (flip Map.lookup names)

nameResults :: Monad m => (Ord k, Ord v) => Map k v -> Map k (PlayerSummary k) -> m (Map v (PlayerSummary v))
nameResults players
  = maybe (fail "Unknown player number") return
  . tournamentNameTraversal (flip Map.lookup players)
