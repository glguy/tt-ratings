module ExportMatches where

import DB
import Control.Lens
import Data.List
import Data.Traversable
import System.Directory
import Data.Maybe
import LawSerialization
import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Time.Calendar

exportMatches :: IO [(Day, [(String,String)])]
exportMatches =
  do dirs <- fmap (\\ [".",".."]) (getDirectoryContents matchDir)
     days <- traverse parseDay dirs
     ms   <- for (sort days) $ \day -> fmap ((,) day) (getMatches day)
     ps   <- loadPlayerMap
     let look p = fromJust $ Map.lookup p ps
     let toTuple (_,m) = (look $ winner m, look $ loser m)
     return $ over (mapped . _2 . mapped) toTuple ms

loadPlayerMap :: IO (Map Int String)
loadPlayerMap = do
  players <- getPlayers
  return (Map.fromList [(playerId player, playerName player) | player <- players])
