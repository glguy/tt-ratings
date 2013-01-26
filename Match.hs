{-# LANGUAGE TemplateHaskell #-}

module Match where

import Control.Lens
import Data.Time

import Player

data Match player = Match
  { _matchWinner, _matchLoser :: player
  , _matchTime :: UTCTime
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Match
