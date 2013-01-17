module Law where

import Data.Array.Unboxed
import Data.List
import Statistics.Distribution
import Statistics.Distribution.Normal

newtype Law = Law (UArray Int Double)

-- | Law assigned to unrated players
defaultLaw :: Law
defaultLaw = normalLaw 1400 450

-- | The list of discrete scores characterized by a law
omega :: [Int]
omega = [0,10..3600]

-- | Generate a normalized law from a list of probabilities
-- which correspond to the elements of 'omega'.
lawFromList :: [Double] -> Law
lawFromList xs = Law $ listArray (0,360) $ fmap (/ sum xs) xs

-- | Find the probability that the score is in the range of
-- [i-5,i+5] given a law.
lawAt :: Law -> Int -> Double
lawAt (Law a) i
  | 0 <= i && i <= 3600 && i `mod` 10 == 0 = a ! (i `div` 10)
  | otherwise = error "lawAt: bad index"

data LawUpdate = LawUpdate
  { playerLaw , opponentLaw :: Law
  , playerWins, playerLoses :: Int
  }

-- | Probability of an upset given the difference in two ratings.
upsetProbability :: Int -> Double
upsetProbability d = recip (1 + exp (alpha * fromIntegral d))
  where
  alpha = 0.0148540595817432

lawUpdate :: LawUpdate -> Law
lawUpdate LawUpdate{playerLaw = lp, opponentLaw = lq, playerWins = w, playerLoses = l}
    = lawFromList
      [ sum [ upsetProbability (q - p) ^ w
            * upsetProbability (p - q) ^ l
            * lawAt lp p
            * lawAt lq q
            | q <- omega]
      | p <- omega
      ]

-- | Generate a discretized normal law given a mean and standard deviation.
normalLaw ::
   Double {- ^ mean -} ->
   Double {- ^ standard deviation -} ->
   Law
normalLaw mean stddev
  = lawFromList
  $ c 5 : [ c (p+5) - c (p-5) | p <- omega \\ [0,3600] ]
 ++ [ complCumulative distr 3595 ]
  where
  distr = normalDistr mean stddev
  c     = cumulative distr . fromIntegral

-- | Compute the mean and standard deviation of a player's law.
lawMeanStddev :: Law -> (Double,Double)
lawMeanStddev law = (mean, sqrt variance)
  where
  mean     = sum [ fromIntegral p       * lawAt law p | p <- omega ]
  variance = sum [ fromIntegral (p ^ 2) * lawAt law p | p <- omega ]
           - mean * mean
