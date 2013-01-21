module LawSerialization where

import Law
import Data.Time.Calendar
import Data.Map (Map)
import qualified Data.List.Split as S
import qualified Data.Map as Map
import Data.Array.Unboxed

serializeLaws :: Map Int (Day, Law) -> String
serializeLaws = unlines . fmap serializeRow . Map.toList

serializeRow :: (Int, (Day, Law)) -> String
serializeRow (i,(day,law))
  = shows i
  . showString ","
  . shows day
  . showString ","
  . shows (lawMean law)
  . showString ","
  . shows (lawStddev law)
  $ foldr (\p -> showString "," . shows p) ""
          (lawElems law)

deserializeLaws :: String -> Maybe (Map Int (Day, Law))
deserializeLaws str = fmap Map.fromList $ mapM deserializeRow (lines str)

deserializeRow :: String -> Maybe (Int, (Day, Law))
deserializeRow str =
  case S.splitOn "," str of
    idStr:dayStr:meanStr:stddevStr:ps
      | length ps == 361 -> do
           day <- parseDay dayStr
           let i = read idStr
               ds = fmap read ps
               law = Law { lawRaw = listArray (0,360) ds
                         , lawMean = read meanStr 
                         , lawStddev = read stddevStr }
           return (i,(day,law))
    _ -> Nothing

parseDay :: Monad m => String -> m Day
parseDay str = case S.splitOn "-" str of
  [y,m,d] -> return $ fromGregorian (read y) (read m) (read d)
  _       -> fail "Unable to parse day"
