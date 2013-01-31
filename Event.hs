{-# LANGUAGE TemplateHaskell #-}

module Event where

import Control.Lens
import Data.Time.Calendar

data Event = Event
  { _eventDay :: Day
  }
  deriving (Show, Ord, Eq)

makeLenses ''Event
