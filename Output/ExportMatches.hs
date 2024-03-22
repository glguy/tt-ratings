module Output.ExportMatches where

import Control.Lens ( view, _Wrapping )
import Data.Traversable ( for )
import Data.Int (Int64)
import NewTTRS.Match ( matchLoser, matchWinner )
import qualified Data.Map as Map

import DataStore ( PlayerId(PlayerId), getEvents, getMatchesByEventId )
import Event ( eventDay )

import Snap.Snaplet.SqliteSimple

exportMatches :: (Applicative m, HasSqlite m) => m [(String, [(Int64, Int64)])]
exportMatches =
  do events <- getEvents
     for (Map.toList events) $ \(eventId,event) ->
       do matches <- fmap Map.elems $ getMatchesByEventId eventId
          return ( show (view eventDay  event)
                 , [ ( view (matchWinner . _Wrapping PlayerId) match
                     , view (matchLoser  . _Wrapping PlayerId) match
                     )
                   | match <- matches
                   ]
                 )
