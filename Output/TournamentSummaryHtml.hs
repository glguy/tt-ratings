{-# LANGUAGE QuasiQuotes #-}
module Output.TournamentSummaryHtml where

import Control.Lens
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Text as Text
import Event
import Law
import Output.Common
import Output.Formatting
import Player
import Text.Hamlet (Html, shamlet)
import Tournament
import qualified Data.Map as Map

tournamentColumns :: Int
tournamentColumns = 2

tournamentHtml :: Event a -> Map Player (PlayerSummary Player) -> Html
tournamentHtml event results = [shamlet|
$doctype 5
$with title <- formatTournamentTitle event
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
        <th>
        <th .colgroup colspan=2>Initial
        <th .colgroup colspan=2>Δ
        <th .colgroup colspan=2>Final
      <tr>
        <th>Name
        <th>μ
        <th>σ
        <th>μ
        <th>σ
        <th>μ
        <th>σ
      $forall (i,(name,summ)) <- itoList $ Map.toList results
         $with (initial,final) <- (view summaryInitialLaw summ, view summaryFinalLaw summ)
          <tr :odd i:.alt>
            <td .opponent>#{view playerName name}
            <td .num .rating>#{showRound $ lawMean   initial}
            <td .num .rating>#{showRound $ lawStddev initial}
            <td .num .delta>^{formatDelta $ lawMean final - lawMean initial}
            <td .num .delta>^{formatDelta $ lawStddev final - lawStddev initial}
            <td .num .rating>#{showRound $ lawMean   final}
            <td .num .rating>#{showRound $ lawStddev final}
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
              <th .colgroup colspan=2>Δ
              <th .colgroup colspan=3>Opponent
              <th .colgroup colspan=2>
            <tr>
              <th>μ
              <th>σ
              <th>μ
              <th>σ
              <th>Name
              <th>W
              <th>L
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

formatTournamentTitle :: Event a -> String
formatTournamentTitle event
  = "Tournament Results - "
 ++ Text.unpack (view eventName event)
 ++ " - "
 ++ formatLongDay (view eventDay event)

formatDelta :: Double -> Html
formatDelta d = case compare d 0 of
  LT -> [shamlet| <span .negative>#{showRound (abs d)} |]
  EQ -> [shamlet| 0 |]
  GT -> [shamlet| #{showRound d} |]

formatDeltaOp :: Double -> String
formatDeltaOp d
  | d >= 0 = " + " ++ showRound d
  | otherwise = " - " ++ showRound (- d)
