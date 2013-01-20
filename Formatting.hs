module Formatting where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

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

formatWeekday :: Int -> String
formatWeekday w = case w of
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"

formatLongDay :: Day -> String
formatLongDay day
  = formatWeekday weekdayNum ++ ", "
  ++ formatMonth monthNum ++ " " ++ show dayNum ++ ", "
  ++ show yearNum
  where
  (yearNum,monthNum,dayNum) = toGregorian day
  (_,_,weekdayNum) = toWeekDate day
