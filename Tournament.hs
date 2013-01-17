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
  { _updatedLaw      :: Law
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

updatePlayer :: Map Name (Map Name Outcome) -> Laws -> Name -> Map Name Outcome -> (Law, Map Name MatchSummary)
updatePlayer outcomes laws player opponents = (myNewLaw, matchSummaries)
  where
  getLaw n = fromMaybe defaultLaw (view (at n) laws)

  myLaw = getLaw player

  matchSummaries = imap computeMatchSummary opponents

  myNewLaw = ifoldl (\opp acc summary -> lawUpdate
                         LawUpdate{playerLaw   = acc
                                  ,opponentLaw = summary^.updatedLaw
                                  ,playerWins  = summary^.summaryOutcome.outcomeWins
                                  ,playerLoses = summary^.summaryOutcome.outcomeLoses})
                     myLaw matchSummaries
               
  computeMatchSummary opp outcome =
    MatchSummary
      { _updatedLaw  = updatedLaw
      , _pointChange = computePointChange myNewLaw updatedLaw outcome
      , _summaryOutcome = outcome
      }
    where
    updatedLaw = computeUpdatedLaw opp player outcomes laws

computeUpdatedLaw :: Name -> Name -> Map Name (Map Name Outcome) -> Laws -> Law
computeUpdatedLaw opp player outcomes laws = ifoldl updateOne (getLaw opp) relevantOutcomes
  where
  getLaw n = fromMaybe defaultLaw (view (at n) laws)
  relevantOutcomes
    = set (at player) Nothing
    $ view (at opp . defaultEmpty) outcomes

  updateOne otherPlayer law outcome =
    lawUpdate LawUpdate{playerLaw   = law
                       ,opponentLaw = getLaw otherPlayer
                       ,playerWins  = outcome^.outcomeWins
                       ,playerLoses = outcome^.outcomeLoses}

computePointChange :: Law -> Law -> Outcome -> Double
computePointChange myLaw hisLaw outcome = newMean - oldMean
  where
  (oldMean,_) = lawMeanStddev myLaw
  (newMean,_) = lawMeanStddev newLaw

  newLaw = lawUpdate LawUpdate{playerLaw   = myLaw
                              ,opponentLaw = hisLaw
                              ,playerWins  = outcome^.outcomeWins
                              ,playerLoses = outcome^.outcomeLoses}


updateLawsForTournament :: Tournament -> Laws -> TournamentSummary
updateLawsForTournament tournament laws = imap (updatePlayer outcomes laws) outcomes
  where
  outcomes = matchOutcomes tournament

defaultEmpty :: SimpleIso (Maybe (Map Name a)) (Map Name a)
defaultEmpty = anon Map.empty Map.null
