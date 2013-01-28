{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
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
import Snap.Snaplet.SqliteSimple (HasSqlite, Sqlite(Sqlite))

import Event
import Player
import DataStore

data Config = Config
  { currentEventId :: EventId
  , resultsFn :: FilePath
  }

runDb :: ReaderT Sqlite IO a -> IO a
runDb m = withConnection "pingpong.db" $ \db ->
  do v <- newMVar db
     runReaderT m $ Sqlite v

main :: IO ()
main = do
  config <- liftIO getConfig
  (event, report) <- runDb $ generateTournamentSummary True $ currentEventId config
  writeFile (resultsFn config) $ renderHtml $ tournamentHtml event report

generateTournamentSummary :: (Applicative m, HasSqlite m) => Bool -> EventId ->
  m (Event EventId, Map Player (PlayerSummary Player))
generateTournamentSummary save eventId = do
  playerMap    <- getPlayers
  Just event   <- getEventById eventId
  previousLaws <- getLawsForEvent eventId
  matches      <- fmap Map.elems $ getMatchesByEventId eventId

  let today           = view eventDay event
      degradedDayLaws = fmap (\(day,law) -> (day, degradeLaw today day law)) previousLaws
      degradedLaws    = fmap snd degradedDayLaws
      results         = evaluateTournament matches degradedLaws

      -- Map.union is left biased, only update laws for those who
      -- played today
      selectLawFromResults s = (today,view summaryFinalLaw s)
      newLawsFromEvent = fmap selectLawFromResults results

  namedResults <- nameResults playerMap results

  when save $ do
    clearLawsForEvent eventId
    ifor_ newLawsFromEvent $ \playerId (_day,law) ->
      addLaw playerId eventId law

  return (event, namedResults)

getConfig :: IO Config
getConfig = do
  args <- getArgs
  case args of
    [currentEventStr, resultsFn] ->
      do let currentEventId = EventId $ read currentEventStr
         return Config {..}
    _ -> fail "usage: tt-ratings EVENTID RESULTS"

nameResults :: Monad m => (Ord k, Ord v) => Map k v ->
  Map k (PlayerSummary k) -> m (Map v (PlayerSummary v))
nameResults players
  = maybe (fail "Unknown player number") return
  . tournamentNameTraversal (flip Map.lookup players)
