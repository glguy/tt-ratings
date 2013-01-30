{-# LANGUAGE RecordWildCards #-}
module TournamentCompiler where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Data.Map (Map)
import Snap.Snaplet.SqliteSimple (HasSqlite)
import qualified Data.Map as Map

import DataStore
import Event
import Player
import Tournament

generateTournamentSummary :: (Applicative m, HasSqlite m) => Bool -> EventId ->
  m (Event, Map Player (PlayerSummary Player))
generateTournamentSummary save eventId = do
  playerMap    <- getPlayers
  Just event   <- getEventById eventId
  previousLaws <- getLawsForEvent True eventId
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

nameResults :: Monad m => (Ord k, Ord v) => Map k v ->
  Map k (PlayerSummary k) -> m (Map v (PlayerSummary v))
nameResults players
  = maybe (fail "Unknown player number") return
  . tournamentNameTraversal (flip Map.lookup players)
