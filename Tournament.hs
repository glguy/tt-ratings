module Tournament where

import Control.Lens
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Law
import qualified Data.Map as Map

data Match name = Match { matchWinner, matchLoser :: name }
  deriving (Eq, Show)

type Tournament name = [Match name]

type OutcomeMap name = Map name (Map name Outcome)

type TournamentSummary name = Map name (Law, Map name MatchSummary)

data MatchSummary = MatchSummary
  { summaryAdjustedLaw  :: Law
  , summaryPointChange  :: Double
  , summaryOutcome      :: Outcome
  }

-- | Outcome without any wins or loses.
noOutcome :: Outcome
noOutcome = Outcome 0 0

-- | Compute a map of outcomes where the first key
-- is the player's name, the second key is the opponent's
-- name and the outcome is from the point of view of the
-- player.
matchOutcomes :: Ord name => Tournament name -> OutcomeMap name
matchOutcomes = foldl' addMatch Map.empty
  where
  look w l = at w . defaultEmpty . at l . non noOutcome

  addMatch outcomes match
    = updateWinner match
    . updateLoser match
    $ outcomes

  updateWinner match
    = look (matchWinner match) (matchLoser  match)
    . outcomeWins
    +~ 1

  updateLoser match
    = look (matchLoser  match) (matchWinner match)
    . outcomeLoses
    +~ 1

-- | Compute a final law and match summary set for a player
-- given the tournament information, initial laws, and player\'s
-- outcomes.
updatePlayer ::
  Ord name =>
  OutcomeMap name  {- ^ Outcome map for the tournament -} ->
  Map name Law     {- ^ Initial laws going into the tournament -} ->
  name             {- ^ Name of player to update -} ->
  Map name Outcome {- ^ outcomes for games played by this player -} ->
  (Law, Map name MatchSummary) {- ^ (Final law, Match Summaries) -}
updatePlayer outcomes laws playerName opponents = (finalLaw, matchSummaries)
  where
  playerInitialLaw = getLaw playerName laws

  (finalLaw, matchSummaries) = imapAccumL computeMatchSummary playerInitialLaw opponents

  computeMatchSummary opponentName accLaw outcome =
    ( lawUpdate u
    , MatchSummary
        { summaryAdjustedLaw    = opponentAdjustedLaw
        , summaryPointChange    = computePointChange u { playerLaw = playerInitialLaw }
        , summaryOutcome        = outcome
        })
    where
    opponentAdjustedLaw = computeAdjustedLaw opponentName playerName outcomes laws
    u = LawUpdate
          { playerLaw   = accLaw
          , opponentLaw = opponentAdjustedLaw
          , updateOutcome = outcome
          }

-- | Compute adjusted law by updating the opponents law for all games
-- which do not include the player.
computeAdjustedLaw ::
  Ord name =>
  name {- ^ Opponent\'s name -} ->
  name {- ^ Player\'s name -} ->
  OutcomeMap name {- ^ Tournament outcomes -} ->
  Map name Law {- ^ Initial laws -} ->
  Law
computeAdjustedLaw opp player outcomes laws
  = ifoldl updateOne (getLaw opp laws) relevantOutcomes
  where
  relevantOutcomes
    = Map.delete player
    $ view (at opp . defaultEmpty) outcomes

  updateOne otherPlayer accLaw outcome =
    lawUpdate LawUpdate
      { playerLaw   = accLaw
      , opponentLaw = getLaw otherPlayer laws
      , updateOutcome = outcome
      }

-- | Estimate the effect that a single pairing had on the player.
computePointChange ::
  LawUpdate ->
  Double {- ^ change in points -}
computePointChange u = newMean - oldMean
  where
  (oldMean,_) = lawMeanStddev (playerLaw u)
  (newMean,_) = lawMeanStddev (lawUpdate u)

degradeLaw ::
  Day {- ^ Today -} ->
  (Day, Law) {- ^ (Last update, law) -} ->
  Law
degradeLaw today (lastUpdate, law) = timeEffect days law
  where
  days = fromIntegral $ diffDays today lastUpdate

degradeLaws :: Functor f => Day -> f (Day, Law) -> f Law
degradeLaws today = fmap (degradeLaw today)

updateLawsForTournament :: Ord name => Tournament name -> Map name Law -> TournamentSummary name
updateLawsForTournament tournament laws = imap (updatePlayer outcomes laws) outcomes
  where
  outcomes = matchOutcomes tournament

defaultEmpty :: Simple Iso (Maybe (Map k v)) (Map k v)
defaultEmpty = anon Map.empty Map.null

getLaw :: Ord name => name -> Map name Law -> Law
getLaw n laws = fromMaybe defaultLaw (view (at n) laws)
