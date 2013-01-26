{-# LANGUAGE TemplateHaskell #-}

module Player where

import Control.Lens
import Data.Text

data Player = Player
  { _playerName :: Text
  }
  deriving (Show, Read, Eq, Ord)

makeLenses ''Player
