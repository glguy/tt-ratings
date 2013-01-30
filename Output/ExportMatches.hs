module Output.ExportMatches where

import Control.Applicative
import Control.Lens
import Data.Traversable
import Data.Int (Int64)
import qualified Data.Map as Map

import DataStore
import Event
import Match

import Snap.Snaplet.SqliteSimple

exportMatches :: (Applicative m, HasSqlite m) => m [(String, [(Int64, Int64)])]
exportMatches =
  do events <- getEvents
     for (Map.toList events) $ \(eventId,event) ->
       do matches <- fmap Map.elems $ getMatchesByEventId eventId
          return ( show (view eventDay  event)
                 , [ ( view (matchWinner . unwrapping PlayerId) match
                     , view (matchLoser  . unwrapping PlayerId) match
                     )
                   | match <- matches
                   ]
                 )
