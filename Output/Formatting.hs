module Output.Formatting where

import Numeric (showFFloat)
import Data.Time.Calendar ( Day, toGregorian )
import Data.Time.Calendar.WeekDate ( toWeekDate )

formatShortDay :: Day -> String
formatShortDay day = formatShortMonth m ++ " " ++ show d
  where
  (_,m,d) = toGregorian day

formatShortMonth :: Int -> String
formatShortMonth m = case m of
    1 -> "Jan"
    2 -> "Feb"
    3 -> "Mar"
    4 -> "Apr"
    5 -> "May"
    6 -> "Jun"
    7 -> "Jul"
    8 -> "Aug"
    9 -> "Sep"
    10 -> "Oct"
    11 -> "Nov"
    12 -> "Dec"
    _  -> error "formatShortMonth: bad month number"

formatMonth :: Int -> String
formatMonth m = case m of
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"
    _  -> error "formatMonth: bad month number"

formatWeekday :: Int -> String
formatWeekday w = case w of
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"
    _  -> error "formatWeekday: bad day number"

formatLongDay :: Day -> String
formatLongDay day
  = formatWeekday weekdayNum ++ ", "
  ++ formatMonth monthNum ++ " " ++ show dayNum ++ ", "
  ++ show yearNum
  where
  (yearNum,monthNum,dayNum) = toGregorian day
  (_,_,weekdayNum) = toWeekDate day

showRound :: Double -> String
showRound x
  | x == 0    = "0"
  | abs x < 1 = showFFloat (Just 1) x ""
  | otherwise = show (round x :: Integer)
