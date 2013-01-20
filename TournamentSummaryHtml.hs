{-# LANGUAGE QuasiQuotes #-}
module TournamentSummaryHtml where

import Law
import Data.List (sortBy)
import Data.Foldable (foldMap)
import Data.Ord (comparing)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List.Split (chunksOf)
import Tournament
import Text.Hamlet (Html, shamlet)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (preEscapedToHtml)

tournamentColumns = 3

ratingsHtml :: Map String (Day,Law) -> String
ratingsHtml laws = renderHtml [shamlet|
$doctype 5
<html>
  <head>
    <meta charset=utf-8>
    <title>Ratings
    <link rel=stylesheet href=ratings.css>
    <script language=javascript src=flot/jquery.js>
    <script language=javascript src=flot/jquery.flot.js>
    <script language=javascript>#{preEscapedToHtml $ graphScript $ map rowLaw rows}
  <body onload="drawGraphs()">
    <h1>Ratings
    <table>
      <tr>
        <th>Player
        <th>Rating
        <th>Metric
        <th>Last played
        <th>Graph
      $forall (i,name,day,law) <- rows
        <tr :odd i:.alt>
          <td>#{name}
          <td>#{formatLaw law}
          <td>#{showRound $ lawScore law}
          <td>#{formatShortDay day}
          <td>
            <div .bargraph #graph#{i}>
  |]
  where
  byScore     = flip (comparing (lawScore . snd . snd))
  rows        = imap mkRow $ sortBy byScore $ Map.toList laws
  mkRow i (name,(day,law)) = (i,name,day,law)
  rowLaw (_,_,_,law) = law

graphScript laws
  = unlines
  $ "function drawGraphs() {"
  : options
  ++ imap drawOne laws
 ++ ["}"]
  where
  drawOne i law = "$.plot($(\"#graph"++show i++"\"), [" ++ show dat ++ "], options);"
    where
    dat = map (\x -> [x,0]) [mkLo law, mkLoMid law, lawMean law, mkHiMid law, mkHi law]
  mkLo law = lawMean law - 2 * lawStddev law
  mkLoMid law = lawMean law - lawStddev law
  mkHiMid law = lawMean law + lawStddev law
  mkHi law = lawMean law + 2 * lawStddev law
  minVal = minimum $ map mkLo laws
  maxVal = maximum $ map mkHi laws
  options =
    ["var options = {"
            ," xaxis: { min: " ++ show minVal ++ ", max: " ++ show maxVal ++  " , show: false },"
            ," yaxis: { show: false },"
            ," margin: { top: 0, left: 0, right: 0, bottom: 0 },"
            ," lines: { show: true },"
            ," points: { show: true, symbol: \"circle\" },"
            ," colors: [\"red\"]"
            ,"};"
            ]

tournamentHtml :: Day -> TournamentSummary String -> String
tournamentHtml day results = renderHtml [shamlet|
$doctype 5
$with title <- formatTournamentTitle day
 <html>
  <head>
    <meta charset=utf-8>
    <title>#{title}
    <link rel=stylesheet type=text/css href=results.css>
  <body>
    <h1>#{title}
    <table .results>
     $forall row <- chunksOf tournamentColumns $ Map.toList results
      <tr>
       $forall (name,summ) <- row
        <td>
         <div .resultbox>
          <span .playername>#{name}
          <span .lawchange>
           #{formatLawChange (view summaryInitialLaw summ) (view summaryFinalLaw summ)}
          <table .matchbox>
            <tr>
              <th>Δ
              <th>Rating
              <th>Opponent
              <th>W
              <th>L
            $forall (i,row) <- itoList $ Map.toList $ view summaryMatches summ
              $with (opponentName,summary) <- row
               <tr :odd i:.alt>
                <td .delta>#{formatDelta $ summaryPointChange summary}
                <td .rating>#{formatLaw $ summaryAdjustedLaw summary}
                <td .opponent>#{opponentName}
                <td .outcome>#{view outcomeWins $ summaryOutcome summary}
                <td .outcome>#{view outcomeLosses $ summaryOutcome summary}
|]

formatShortDay :: Day -> String
formatShortDay day = formatShortMonth m ++ " " ++ show d
  where
  (_,m,d) = toGregorian day

formatTournamentTitle :: Day -> String
formatTournamentTitle day
  = "Tournament Results - "
  ++ weekday ++ ", "
  ++ month ++ " " ++ show dayNum ++ ", "
  ++ show yearNum
  where
  (yearNum,monthNum,dayNum) = toGregorian day
  (_,_,weekdayNum) = toWeekDate day

  month = formatMonth monthNum
  weekday = formatWeekday weekdayNum

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

showRound :: Double -> String
showRound x = show (round x :: Integer)

formatLaw :: Law -> String
formatLaw law = showRound (lawMean law) ++ "±" ++ showRound (lawStddev law)

formatDelta :: Double -> String
formatDelta d
  | d > 0 = '+':showRound d
  | otherwise = showRound d

formatDeltaOp :: Double -> String
formatDeltaOp d
  | d >= 0 = " + " ++ showRound d
  | otherwise = " - " ++ showRound (- d)

-- | Render the change between an old law and a new law.
formatLawChange ::
  Law {- ^ Old law -} ->
  Law {- ^ New law -} ->
  String
formatLawChange old new =
  formatLaw old
  ++ formatDeltaOp (lawMean new - lawMean old) ++ " = "
  ++ formatLaw new
