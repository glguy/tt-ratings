{-# LANGUAGE QuasiQuotes #-}
module TournamentSummaryHtml where

import Law
import Formatting
import Data.List (sortBy)
import Data.Foldable (foldMap)
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

tournamentColumns = 2

ratingsHtml :: Day -> Map String (Day,Law) -> String
ratingsHtml day laws = renderHtml [shamlet|
$doctype 5
<html>
  <head>
    <meta charset=utf-8>
    <title>#{title}
    <link rel=stylesheet href=ratings.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
    <script language=javascript src=flot/jquery.flot.js>
    <script language=javascript src=jquery.tablesorter.min.js>
    <script language=javascript>#{preEscapedToHtml $ graphScript $ map rowLaw rows}
    <script language=javascript>$(document).ready(function(){ $("#players-table").tablesorter();});
  <body>
    <h1>#{title}
    <table #players-table>
      <thead>
       <tr>
         <th>Player
         <th>μ
         <th>σ
         <th>Metric
         <th>Last played
         <th>Graph
      <tbody>
        $forall (i,name,day,law) <- rows
          <tr :odd i:.alt>
            <td .str>#{name}
            <td .num>#{showRound $ lawMean law}
            <td .num>#{showRound $ lawStddev law}
            <td .num>#{showRound $ lawScore law}
            <td .str>#{formatShortDay day}
            <td>
              <div .bargraph #graph#{i}>
  |]
  where
  title       = "Player List - " ++ formatLongDay day
  byScore     = flip (comparing (lawScore . snd . snd))
  rows        = imap mkRow $ sortBy byScore $ Map.toList laws
  mkRow i (name,(day,law)) = (i,name,day,law)
  rowLaw (_,_,_,law) = law

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

tournamentHtml :: Day -> Map String (PlayerSummary String) -> String
tournamentHtml day results = renderHtml [shamlet|
$doctype 5
$with title <- formatTournamentTitle day
 <html>
  <head>
    <meta charset=utf-8>
    <title>#{title}
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
    <table .summary>
      <tr>
        <th>Name
        <th>Initial
        <th>Δ
        <th>Final
      $forall (i,row) <- rows
        $with (name,summ) <- row
         $with (initial,final) <- (view summaryInitialLaw summ, view summaryFinalLaw summ)
          <tr :odd i:.alt>
            <td .opponent>#{name}
            <td .rating>#{formatLaw   initial}
            <td .delta>^{formatDelta $ lawMean final - lawMean initial}
            <td .rating>#{formatLaw   final}
|]
    where rows = itoList $ sortBy (flip (comparing (lawMean . view summaryFinalLaw . snd))) $ Map.toList results
  detailed = [shamlet|
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
              <th>μ
              <th>σ
              <th>Opponent
              <th>W
              <th>L
            $forall (i,row) <- itoList $ Map.toList $ view summaryMatches summ
              $with (opponentName,summary) <- row
               <tr :odd i:.alt>
                <td .delta>^{formatDelta $ summaryPointChange summary}
                <td .rating>#{showRound $ lawMean   $ summaryAdjustedLaw summary}
                <td .rating>#{showRound $ lawStddev $ summaryAdjustedLaw summary}
                <td .opponent>#{opponentName}
                <td .outcome>#{view outcomeWins $ summaryOutcome summary}
                <td .outcome>#{view outcomeLosses $ summaryOutcome summary}
|]

formatTournamentTitle :: Day -> String
formatTournamentTitle day = "Tournament Results - " ++ formatLongDay day

showRound :: Double -> String
showRound x = show (round x :: Integer)

formatLaw :: Law -> String
formatLaw law = showRound (lawMean law) ++ "±" ++ showRound (lawStddev law)

formatDelta :: Double -> Html
formatDelta d = case compare d 0 of
  LT -> [shamlet| <span .negative>#{showRound d} |]
  EQ -> [shamlet| 0 |]
  GT -> [shamlet| +#{showRound d} |]

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
