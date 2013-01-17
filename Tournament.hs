{-# LANGUAGE TemplateHaskell #-}
module Tournament where

import Law
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Control.Lens

type Name = String

data Match = Match { _matchWinner, _matchLoser :: Name }
  deriving (Eq, Show)

type Tournament = [Match]

type Laws = Map Name Law

data Outcome = Outcome { _outcomeWins, _outcomeLoses :: Int }
  deriving (Eq, Show)

type TournamentSummary = Map Name (Law, Map Name MatchSummary)

data MatchSummary = MatchSummary
  { _adjustedLaw     :: Law
  , _pointChange     :: Double
  , _summaryOutcome  :: Outcome
  }

makeLenses ''MatchSummary
makeLenses ''Match
makeLenses ''Outcome

-- | Outcome without any wins or loses.
noOutcome :: Outcome
noOutcome = Outcome 0 0

-- | Compute a map of outcomes where the first key
-- is the player's name, the second key is the opponent's
-- name and the outcome is from the point of view of the
-- player.
matchOutcomes :: Tournament -> Map Name (Map Name Outcome)
matchOutcomes = foldl' addMatch Map.empty
  where
  look1 n = at n . defaultEmpty
  look2 n = at n . non noOutcome

  addMatch outcomes match
    = updateWinner match
    . updateLoser match
    $ outcomes

  updateWinner match
    = look1 (match^.matchWinner)
    . look2 (match^.matchLoser)
    . outcomeWins
    +~ 1

  updateLoser match
    = look1 (match^.matchLoser)
    . look2 (match^.matchWinner)
    . outcomeLoses
    +~ 1

-- | Compute a final law and match summary set for a player
-- given the tournament information, initial laws, and player\'s
-- outcomes.
updatePlayer ::
  Map Name (Map Name Outcome) {- ^ Outcome map for the tournament -} ->
  Laws {- ^ Initial laws going into the tournament -} ->
  Name {- ^ Name of player to update -} ->
  Map Name Outcome {- ^ outcomes for games played by this player -} ->
  (Law, Map Name MatchSummary)
updatePlayer outcomes laws playerName opponents = (finalLaw, matchSummaries)
  where
  getLaw n = fromMaybe defaultLaw (view (at n) laws)

  initialLaw = getLaw playerName

  (finalLaw, matchSummaries) = imapAccumL computeMatchSummary initialLaw opponents

  computeMatchSummary opponentName accLaw outcome =
    ( accLaw'
    , MatchSummary
        { _adjustedLaw    = adjustedLaw
        , _pointChange    = computePointChange initialLaw adjustedLaw outcome
        , _summaryOutcome = outcome
        })
    where
    adjustedLaw = computeAdjustedLaw opponentName playerName outcomes laws
    accLaw' = lawUpdate LawUpdate
               { playerLaw   = accLaw
               , opponentLaw = adjustedLaw
               , playerWins  = outcome^.outcomeWins
               , playerLoses = outcome^.outcomeLoses
               }

-- | Compute adjusted law by updating the opponents law for all games
-- which do not include the player.
computeAdjustedLaw ::
  Name {- ^ Opponent\'s name -} ->
  Name {- ^ Player\'s name -} ->
  Map Name (Map Name Outcome) {- ^ Tournament outcomes -} ->
  Laws {- ^ Initial laws -} ->
  Law
computeAdjustedLaw opp player outcomes laws
  = ifoldl updateOne (getLaw opp) relevantOutcomes
  where
  getLaw n = fromMaybe defaultLaw (view (at n) laws)

  relevantOutcomes
    = Map.delete player
    $ view (at opp . defaultEmpty) outcomes

  updateOne otherPlayer accLaw outcome =
    lawUpdate LawUpdate{playerLaw   = accLaw
                       ,opponentLaw = getLaw otherPlayer
                       ,playerWins  = outcome^.outcomeWins
                       ,playerLoses = outcome^.outcomeLoses}

-- | Estimate the effect that a single pairing had on the player.
computePointChange ::
  Law {- ^ Player's initial law -} ->
  Law {- ^ Opponent's adjusted law -} ->
  Outcome {- ^ outcome between player and opponent -} ->
  Double {- ^ change in points -}
computePointChange myLaw hisLaw outcome = newMean - oldMean
  where
  (oldMean,_) = lawMeanStddev myLaw
  (newMean,_) = lawMeanStddev newLaw

  newLaw      = lawUpdate LawUpdate
                  { playerLaw   = myLaw
                  , opponentLaw = hisLaw
                  , playerWins  = outcome^.outcomeWins
                  , playerLoses = outcome^.outcomeLoses
                  }

updateLawsForTournament :: Tournament -> Laws -> TournamentSummary
updateLawsForTournament tournament laws = imap (updatePlayer outcomes laws) outcomes
  where
  outcomes = matchOutcomes tournament

defaultEmpty :: SimpleIso (Maybe (Map Name a)) (Map Name a)
defaultEmpty = anon Map.empty Map.null
