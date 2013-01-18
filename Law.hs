{-# LANGUAGE TemplateHaskell #-}
module Law where

import Control.Lens
import Data.Array.Unboxed
import Data.List
import Statistics.Distribution (complCumulative, cumulative)
import Statistics.Distribution.Normal (normalDistr)

data Law = Law { lawRaw :: !(UArray Int Double)
               , lawMean, lawStddev :: !Double }

data Outcome = Outcome { _outcomeWins, _outcomeLoses :: Int }
  deriving (Eq, Show)

makeLenses ''Outcome

data LawUpdate = LawUpdate
  { playerLaw , opponentLaw :: Law
  , updateOutcome :: Outcome
  }


-- | Law assigned to unrated players
defaultLaw :: Law
defaultLaw = normalLaw 1400 450

-- | The list of discrete scores characterized by a law
omega :: [Int]
omega = [0,10..3600]

-- | Generate a normalized law from a list of probabilities
-- which correspond to the elements of 'omega'.
lawFromList :: [Double] -> Law
lawFromList xs = Law (listArray (0,360) normalized) mean (sqrt variance)
  where
  normalized = fmap (/ sum xs) xs
  mean     = sum [ fromIntegral p       * x | (p,x) <- zip omega normalized ]
  variance = sum [ fromIntegral (p * p) * x | (p,x) <- zip omega normalized ]
           - mean * mean


-- | Find the probability that the score is in the range of
-- [i-5,i+5] given a law.
lawAt :: Law -> Int -> Double
lawAt law i
  | 0 <= i && i <= 3600 && i `mod` 10 == 0 = lawRaw law ! (i `div` 10)
  | otherwise = error "lawAt: bad index"

-- | Probability of an upset given the difference in two ratings.
upsetProbability :: Int -> Double
upsetProbability d = recip (1 + exp (alpha * fromIntegral d))
  where
  alpha = 0.0148540595817432

lawUpdate :: LawUpdate -> Law
lawUpdate LawUpdate{playerLaw = lp, opponentLaw = lq, updateOutcome = outcome}
    = lawFromList
      [ sum [ upsetProbability (q - p) ^ w
            * upsetProbability (p - q) ^ l
            * lawAt lp p
            * lawAt lq q
            | q <- omega]
      | p <- omega
      ]
  where
  w = view outcomeWins outcome
  l = view outcomeLoses outcome

-- | Generate a discretized normal law given a mean and standard deviation.
normalLaw ::
   Double {- ^ mean -} ->
   Double {- ^ standard deviation -} ->
   Law
normalLaw mean stddev = lawFromList $ mkNormal 0 3600 mean stddev

mkNormal :: Int -> Int -> Double -> Double -> [Double]
mkNormal lo hi mean stddev =
  c (lo + 5) : [ c (p+5) - c (p-5) | p <- [lo+10, lo+20 .. hi - 10]]
 ++ [ complCumulative distr (fromIntegral hi - 5) ]
  where
  distr = normalDistr mean stddev
  c     = cumulative distr . fromIntegral

-- | Degrade a law given a certain number of days. When days is
-- less than 1, no degrading is done.
timeEffect :: Int -> Law -> Law
timeEffect days law
  | days <= 0 = law
  | otherwise = lawFromList
              $ sum [ lawAt law x * timeAt y | x <- omega, y <- [-3600,-3590.. -x]]
              : [ sum [ lawAt law x * timeAt (r - x) | x <- omega ]
                | r <- omega \\ [0,3600] ]
             ++ [ sum [lawAt law x * timeAt y | x <- omega, y <- [3600-x,3610-x..3600]] ]
  where
  timeArray :: UArray Int Double
  timeArray = listArray (-360,360) $ mkNormal (-3600) 3600 0 (70 * fromIntegral days / 365)
  timeAt y = timeArray ! (y `div` 10)

lawScore :: Law -> Double
lawScore law = lawMean law - 2 * lawStddev law

lawIsRated :: Law -> Bool
lawIsRated l = lawStddev l < 100
