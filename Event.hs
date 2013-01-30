{-# LANGUAGE TemplateHaskell #-}

module Event where

import Control.Lens
import Data.Time.Calendar
import Data.Text

data Event = Event
  { _eventDay :: Day
  }
  deriving (Show, Ord, Eq)

makeLenses ''Event
