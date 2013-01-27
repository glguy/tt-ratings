module Output.ExportMatches where

import Control.Lens
import Data.Map ( Map )
import Data.Time.Calendar
import Data.Traversable
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map

import DataStore
import Event
import Match

exportMatches :: DatabaseM m => m [(Text, String, [(Int64, Int64)])]
exportMatches =
  do events <- getEvents
     for (Map.toList events) $ \(eventId,event) ->
       do matches <- fmap Map.elems $ getMatchesByEventId eventId
          return ( view eventName event
                 , show (view eventDay  event)
                 , [ ( view (matchWinner . unwrapping PlayerId) match
                     , view (matchLoser  . unwrapping PlayerId) match
                     )
                   | match <- matches
                   ]
                 )
