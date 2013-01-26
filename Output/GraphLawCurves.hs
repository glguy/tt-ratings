module Output.GraphLawCurves where

import Data.List
import qualified Data.Map as Map
import Data.Map ( Map )
import Law
import Data.List (sortBy)
import Data.Ord (comparing)

generateFlotData :: Map String Law -> String
generateFlotData laws = "var data = [ " ++ intercalate "  \n," (map generatePlayerData (Map.toList laws)) ++ " ];"

generatePlayerData :: (String, Law) -> String
generatePlayerData (name,law) = "player(" ++ show name ++ ", " ++ generateLawData law ++ ")"

generateLawData law = show (zipWith (\k v -> [fromIntegral k,v]) omega (lawElems law))

generateSimpleFlotData :: Map String Law -> String
generateSimpleFlotData laws = "var data = [ " ++ intercalate "  \n," (map generateSimplePlayerData $ sortBy (comparing (lawScore.snd)) (Map.toList laws)) ++ " ];"

generateSimplePlayerData :: (String, Law) -> String
generateSimplePlayerData (name,law) = "player(" ++ show name ++ ", " ++ show (lawMean law) ++ ", " ++ show (lawScore law) ++ ", " ++ show (lawStddev law) ++ ")"
