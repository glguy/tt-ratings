{-# LANGUAGE QuasiQuotes #-}
module Output.TournamentSummaryHtml where

import Law
import Player
import Output.Formatting
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Calendar
import Data.List.Split (chunksOf)
import Tournament
import Text.Hamlet (Html, shamlet)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (preEscapedToHtml)

tournamentColumns :: Int
tournamentColumns = 2

metaTags :: Html
metaTags = [shamlet|
  <meta charset="UTF-8" />
  <meta name="google" content="notranslate">
  <meta http-equiv="Content-Language" content="en" />
|]

ratingsHtml :: Day -> Map Player (Day,Law) -> String
ratingsHtml day laws = renderHtml [shamlet|
$doctype 5
<html>
  <head>
    ^{metaTags}
    <title>#{title}
    <link rel=stylesheet href=common.css>
    <link rel=stylesheet href=ratings.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
    <script language=javascript src=flot/jquery.flot.js>
    <script language=javascript src=jquery.tablesorter.min.js>
    <script language=javascript>#{preEscapedToHtml $ graphScript $ map rowLaw rows}
    <script language=javascript>$(document).ready(function(){ $("#players-table").tablesorter();});
  <body>
    <h1>#{title}
    <table #players-table .data>
      <thead>
       <tr>
         <th>Player
         <th>μ
         <th>σ
         <th>Metric
         <th>Last played
         <th>Graph
      <tbody>
        $forall (i,name,lastday,law) <- rows
          <tr :odd i:.alt>
            <td .str>#{view playerName name}
            <td .num>#{showRound $ lawMean law}
            <td .num>#{showRound $ lawStddev law}
            <td .num>#{showRound $ lawScore law}
            <td .str>
              $if day == lastday
                <i>today
              $else
                #{formatShortDay lastday}
            <td>
              <div .bargraph #graph#{i}>
  |]
  where
  title       = "Player List - " ++ formatLongDay day
  byScore     = flip (comparing (lawScore . snd . snd))
  rows        = imap mkRow $ sortBy byScore $ Map.toList laws
  mkRow i (name,(d,law)) = (i,name,d,law)
  rowLaw (_,_,_,law) = law

graphScript :: [Law] -> String
graphScript laws
  = unlines
  $ "$(document).ready(function drawGraphs() {"
  : options
  ++ imap drawOne laws
 ++ ["})"]
  where
  drawOne i law =
    "$.plot($(\"#graph"++show i++"\"), [" ++ show dat ++ "], options);"
    where
    dat = map (\x -> [x,0]) [mkLo law, mkLoMid law, lawMean law, mkHiMid law, mkHi law]
  mkLo law = lawMean law - 2 * lawStddev law
  mkLoMid law = lawMean law - lawStddev law
  mkHiMid law = lawMean law + lawStddev law
  mkHi law = lawMean law + 2 * lawStddev law
  minVal = minimum $ map mkLo laws
  maxVal = maximum $ map mkHi laws
  options =
      [ "function vertLine(ctx, x, y, radius, shadow) {"
      , "     ctx.moveTo(x, y - radius);"
      , "     ctx.lineTo(x, y + radius);"
      , "}"
      , "var options = {"
      ," xaxis: { min: " ++ show minVal ++ ", max: " ++ show maxVal ++  " , show: false },"
      ," yaxis: { show: false },"
      ," margin: { top: 0, left: 0, right: 0, bottom: 0 },"
      ," lines: { show: true },"
      ," points: { show: true, symbol: vertLine },"
      ," colors: [\"red\"]"
      ,"};"
      ]

tournamentHtml :: Day -> Map Player (PlayerSummary Player) -> String
tournamentHtml day results = renderHtml [shamlet|
$doctype 5
$with title <- formatTournamentTitle day
 <html>
  <head>
    ^{metaTags}
    <title>#{title}
    <link rel=stylesheet type=text/css href=common.css>
    <link rel=stylesheet type=text/css href=results.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
  <body>
    <h1>#{title}
    <div #tabs>
      <div #summary-report>
        ^{summary}
      <div #detailed-report>
        ^{detailed}
|]
  where
  summary = [shamlet|
    <table .summary .data>
      <tr>
        <th rowspan=2>Name
        <th .colgroup colspan=2>Initial
        <th rowspan=2>Δμ
        <th rowspan=2>Δσ
        <th .colgroup colspan=2>Final
      <tr>
        <th>μ
        <th>σ
        <th>μ
        <th>σ
      $forall (i,(name,summ)) <- itoList $ Map.toList results
         $with (initial,final) <- (view summaryInitialLaw summ, view summaryFinalLaw summ)
          <tr :odd i:.alt>
            <td .opponent>#{view playerName name}
            <td .rating>#{showRound $ lawMean   initial}
            <td .rating>#{showRound $ lawStddev initial}
            <td .delta>^{formatDelta $ lawMean final - lawMean initial}
            <td .delta>^{formatDelta $ lawStddev final - lawStddev initial}
            <td .rating>#{showRound $ lawMean   final}
            <td .rating>#{showRound $ lawStddev final}
|]

  detailed = [shamlet|
    <table .results>
     $forall row <- chunksOf tournamentColumns $ Map.toList results
      <tr>
       $forall (name,summ) <- row
        <td>
         <div .resultbox>
          <span .playername>#{view playerName name}
          <table .matchbox .data>
            <tr>
              <th rowspan=2>Δμ
              <th rowspan=2>Δσ
              <th .colgroup colspan=3>Opponent
              <th rowspan=2>W
              <th rowspan=2>L
            <tr>
              <th>μ
              <th>σ
              <th>Name
            $forall (i,(opponentName,summary)) <- itoList $ Map.toList $ view summaryMatches summ
               <tr :odd i:.alt>
                <td .delta>^{formatDelta $ summaryMeanChange   summary}
                <td .delta>^{formatDelta $ summaryStddevChange summary}
                <td .quiet .rating>#{showRound $ lawMean   $ summaryAdjustedLaw summary}
                <td .quiet .rating>#{showRound $ lawStddev $ summaryAdjustedLaw summary}
                <td .opponent>#{view playerName opponentName}
                $with o <- summaryOutcome summary
                  $with (w,l) <- (view outcomeWins o, view outcomeLosses o)
                    <td :isZero w:.quiet .outcome>#{w}
                    <td :isZero l:.quiet .outcome>#{l}
|]

isZero :: Int -> Bool
isZero z = z == 0

formatTournamentTitle :: Day -> String
formatTournamentTitle day = "Tournament Results - " ++ formatLongDay day

formatDelta :: Double -> Html
formatDelta d = case compare d 0 of
  LT -> [shamlet| <span .negative>#{showRound (abs d)} |]
  EQ -> [shamlet| 0 |]
  GT -> [shamlet| #{showRound d} |]

formatDeltaOp :: Double -> String
formatDeltaOp d
  | d >= 0 = " + " ++ showRound d
  | otherwise = " - " ++ showRound (- d)
