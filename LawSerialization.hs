module LawSerialization where

import Data.List.Split (splitOn)
import Data.Array.Unboxed (listArray)
import NewTTRS.Law

serializeLaw :: Law -> String
serializeLaw law
  = shows (lawMean law)
  . showString ","
  . shows (lawStddev law)
  $ foldr (\p -> showString "," . shows p) ""
          (lawElems law)

deserializeLaw :: String -> Maybe Law
deserializeLaw str =
  case splitOn "," str of
    meanStr:stddevStr:ps
      | length ps == 361 -> do
           let ds = fmap read ps
           return $ Law { lawRaw = listArray (0,360) ds
                         , lawMean = read meanStr
                         , lawStddev = read stddevStr }
    _ -> Nothing
