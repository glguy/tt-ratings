{-# LANGUAGE TemplateHaskell #-}

module Match where

import Control.Applicative
import Control.Lens
import Data.Time
import Data.Foldable
import Data.Traversable

data Match player = Match
  { _matchWinner, _matchLoser :: player
  , _matchTime :: UTCTime
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Match

instance Functor Match where fmap = fmapDefault
instance Foldable Match where foldMap = foldMapDefault
instance Traversable Match where
  traverse f m = (\w l -> m { _matchWinner = w, _matchLoser  = l})
              <$> f (_matchWinner m)
              <*> f (_matchLoser  m)
