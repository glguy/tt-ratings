module Output.GraphLawCurves where

import Control.Lens (view)
import Data.List (intercalate)
import Data.List (sortBy)
import Data.Map ( Map )
import Data.Ord (comparing)
import Law
import qualified Data.Map as Map

import Player

generateFlotData :: Map Player Law -> String
generateFlotData laws = "var data = [ " ++ intercalate "  \n," (map generatePlayerData (Map.toList laws)) ++ " ];"

generatePlayerData :: (Player, Law) -> String
generatePlayerData (player,law) = "player(" ++ show (view playerName player) ++ ", " ++ generateLawData law ++ ")"

generateLawData :: Law -> String
generateLawData law = show (zipWith (\k v -> [fromIntegral k,v]) omega (lawElems law))

generateSimpleFlotData :: Map String Law -> String
generateSimpleFlotData laws = "var data = [ " ++ intercalate "  \n," (map generateSimplePlayerData $ sortBy (comparing (lawScore.snd)) (Map.toList laws)) ++ " ];"

generateSimplePlayerData :: (String, Law) -> String
generateSimplePlayerData (name,law) = "player(" ++ show name ++ ", " ++ show (lawMean law) ++ ", " ++ show (lawScore law) ++ ", " ++ show (lawStddev law) ++ ")"
