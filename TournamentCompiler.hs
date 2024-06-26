{-# LANGUAGE RecordWildCards #-}
module TournamentCompiler where

import Control.Monad ( when )
import Control.Lens ( view, ifor_ )
import Data.Map (Map)
import NewTTRS.Tournament ( PlayerSummary, summaryFinalLaw, degradeLaw, evaluateTournament )
import Snap.Snaplet.SqliteSimple (HasSqlite)
import qualified Data.Map as Map

import DataStore ( PlayerId, EventId, getEventById, getMatchesByEventId, getLawsForEvent, clearLawsForEvent, addLaw )
import Event ( Event, eventDay )

generateTournamentSummary ::
  (MonadFail m, HasSqlite m) =>
  Bool -> EventId -> m (Event, Map PlayerId (PlayerSummary PlayerId))
generateTournamentSummary save eventId = do
  Just event <- getEventById eventId
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

  when save $ do
    clearLawsForEvent eventId
    ifor_ newLawsFromEvent $ \playerId (_day,law) ->
      addLaw playerId eventId law

  return (event, results)
