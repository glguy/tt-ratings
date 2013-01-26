{-# LANGUAGE TemplateHaskell #-}

module Event where

import Control.Lens
import Data.Time.Calendar
import Data.Text

data Event event = Event
  { _eventName :: Text
  , _eventDay :: Day
  , _eventActive :: Bool
  , _eventPrevious :: Maybe event
  }
  deriving (Show, Ord, Eq)

makeLenses ''Event
