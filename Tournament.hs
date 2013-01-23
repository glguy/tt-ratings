{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Tournament where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Lens
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Data.Traversable
import DB (Match(..))
import Law
import qualified Data.Map as Map

data PlayerSummary name = PlayerSummary
  { _summaryInitialLaw, _summaryFinalLaw :: Law
  , _summaryMatches :: Map name MatchSummary
  }

data MatchSummary = MatchSummary
  { summaryAdjustedLaw  :: Law
  , summaryMeanChange, summaryStddevChange :: Double
  , summaryOutcome      :: Outcome
  }

makeLenses ''PlayerSummary

-- | Outcome without any wins or loses.
noOutcome :: Outcome
noOutcome = Outcome 0 0

-- | Compute a map of outcomes where the first key
-- is the player's name, the second key is the opponent's
-- name and the outcome is from the point of view of the
-- player.
matchOutcomes :: Ord name => [Match name] -> Map name (Map name Outcome)
matchOutcomes = foldl' addMatch Map.empty
  where
  look w l = at w . defaultEmpty . at l . non noOutcome

  addMatch outcomes match = updateWinner match . updateLoser match $ outcomes
  updateWinner match = look (winner match) (loser  match) . outcomeWins   +~ 1
  updateLoser  match = look (loser  match) (winner match) . outcomeLosses +~ 1

-- | Compute a final law and match summary set for a player
-- given the tournament information, initial laws, and player\'s
-- outcomes.
updatePlayer ::
  Ord name =>
  Map name Law     {- ^ Nearly adjusted laws -} ->
  Map name (Map name Outcome)  {- ^ Outcome map for the tournament -} ->
  Map name Law     {- ^ Initial laws going into the tournament -} ->
  name             {- ^ Name of player to update -} ->
  Map name Outcome {- ^ outcomes for games played by this player -} ->
  PlayerSummary name
updatePlayer nearlyAdjustedLaws outcomes laws playerName opponents
  = PlayerSummary
      { _summaryInitialLaw = initialLaw
      , _summaryFinalLaw = finalLaw
      , _summaryMatches = matchSummaries
      }
  where
  initialLaw = getLaw playerName laws

  (finalLaw, matchSummaries) = imapAccumL computeMatchSummary initialLaw opponents

  computeMatchSummary opponentName accLaw outcome =
    ( finalLaw
    , MatchSummary
        { summaryAdjustedLaw    = opponentAdjustedLaw
        , summaryMeanChange     = lawMean   finalLaw - lawMean   accLaw
        , summaryStddevChange   = lawStddev finalLaw - lawStddev accLaw
        , summaryOutcome        = outcome
        })
    where
    opponentNearlyAdjustedLaw = fromMaybe (error "missing nearly adjusted law")
                              $ Map.lookup opponentName nearlyAdjustedLaws
    opponentAdjustedLaw = lawUnupdate LawUpdate
          { playerLaw   = opponentNearlyAdjustedLaw
          , opponentLaw = initialLaw
          , updateOutcome = flipOutcome outcome
          }
    finalLaw = lawUpdate LawUpdate
          { playerLaw   = accLaw
          , opponentLaw = opponentAdjustedLaw
          , updateOutcome = outcome
          }

-- | Estimate the effect that a single pairing had on the player.
computePointChange ::
  LawUpdate ->
  Double {- ^ change in points -}
computePointChange u = lawMean (lawUpdate u) - lawMean (playerLaw u)

degradeLaw ::
  Day {- ^ Today -} ->
  Day  {- ^ Last update -} ->
  Law ->
  Law
degradeLaw today lastUpdate law = timeEffect days law
  where
  days = fromIntegral $ diffDays today lastUpdate

evaluateTournament :: Ord name => [Match name] -> Map name Law -> Map name (PlayerSummary name)
evaluateTournament tournament laws = imap (updatePlayer firstPassLaws outcomes laws) outcomes
  where
  outcomes = matchOutcomes tournament
  firstPassLaws = imap (firstPass laws) outcomes

firstPass :: Ord name => Map name Law -> name -> Map name Outcome -> Law
firstPass initialLaws name = ifoldl aux (getLaw name initialLaws)
  where
  aux otherPlayer accLaw outcome =
    lawUpdate LawUpdate
      { playerLaw   = accLaw
      , opponentLaw = getLaw otherPlayer initialLaws
      , updateOutcome = outcome
      }

defaultEmpty :: Iso' (Maybe (Map k v)) (Map k v)
defaultEmpty = anon Map.empty Map.null

getLaw :: Ord name => name -> Map name Law -> Law
getLaw n m = fromMaybe defaultLaw $ Map.lookup n m

traverseKeys :: Ord b => Traversal (Map a v) (Map b v) a b
traverseKeys f = fmap Map.fromList . (traverse . _1) f . Map.toList

tournamentNameTraversal :: Ord b =>
  Traversal (Map a (PlayerSummary a)) (Map b (PlayerSummary b)) a b
tournamentNameTraversal f = fmap Map.fromList . traverse fixEntry . Map.toList
  where
  fixEntry = beside id (summaryMatches . traverseKeys) f
