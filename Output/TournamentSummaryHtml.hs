{-# LANGUAGE QuasiQuotes #-}
module Output.TournamentSummaryHtml where

import Law
import Player
import Output.Formatting
import Data.Time.Calendar
import Data.List.Split (chunksOf)
import Tournament
import Text.Hamlet (Html, shamlet)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Output.Common

tournamentColumns :: Int
tournamentColumns = 2

tournamentHtml :: Day -> Map Player (PlayerSummary Player) -> Html
tournamentHtml day results = [shamlet|
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
