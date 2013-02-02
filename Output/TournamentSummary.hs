{-# LANGUAGE QuasiQuotes #-}
module Output.TournamentSummary where

import Control.Lens
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Ord (comparing)
import Text.Hamlet (Html, shamlet)
import qualified Data.Map as Map

import DataStore
import Event
import Law
import Output.Common
import Output.Formatting
import Player
import Tournament

tournamentColumns :: Int
tournamentColumns = 2

tournamentHtml :: Map PlayerId Player -> Event -> Map PlayerId (PlayerSummary PlayerId) -> Html
tournamentHtml players event results = [shamlet|
$doctype 5
$with title <- formatTournamentTitle event
 <html>
  <head>
    ^{metaTags}
    <title>#{title}
    <link rel=stylesheet type=text/css href=/static/common.css>
    <link rel=stylesheet type=text/css href=/static/results.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
    <script language=javascript src=/static/jquery.flot.js>
    ^{graphInclude sortedLaws}
  <body>
    ^{navigationLinks}
    <h1>#{title}
    <div #tabs>
      <div #summary-report>
        ^{summary}
      <div #detailed-report>
        ^{detailed}
|]
  where
  sorted = sortBy cmp $ Map.toList results
  cmp    = flip $ comparing $ view $ _2 . summaryFinalLaw . to lawScore
  sortedLaws = map (view (_2 . summaryFinalLaw)) sorted
  summary = [shamlet|
    <table .summary .data>
      <tr>
        <th>
        <th .colgroup colspan=3>Δ
        <th .colgroup colspan=4>Final
      <tr>
        <th>Name
        <th>μ
        <th>σ
        <th>#{"#"}
        <th>μ
        <th>σ
        <th>#{"#"}
        <th>Graph
      $forall (i,(playerId,summ)) <- itoList sorted
         $with (initial,final) <- (view summaryInitialLaw summ, view summaryFinalLaw summ)
          <tr :odd i:.alt>
            <td .opponent>
              $with Just player <- view (at playerId) players
                ^{playerLink playerId player}
            <td .num .delta>^{formatDelta $ lawMean final - lawMean initial}
            <td .num .delta>^{formatDelta $ lawStddev final - lawStddev initial}
            $with rankChange <- negate $ view summaryFinalRank summ - view summaryInitialRank summ
              <td .num .delta>
                $if not $ isZero rankChange
                  ^{formatDelta $ fromIntegral rankChange}
            <td .num .rating>#{showRound $ lawMean   final}
            <td .num .rating>#{showRound $ lawStddev final}
            <td .num .rating>#{1 + view summaryFinalRank summ}
            <td>
              <div .bargraph #graph#{i}>
|]

  countWins   = sumOf (summaryMatches . folded . summaryOutcome . outcomeWins)
  countLosses = sumOf (summaryMatches . folded . summaryOutcome . outcomeLosses)
  detailed = [shamlet|
<table .results>
  $forall row <- chunksOf tournamentColumns $ Map.toList results
    <tr>
      $forall (playerId,summ) <- row
        $with Just player <- view (at playerId) players
          <td>
           <div .resultbox>
            <span .playername>
                ^{playerLink playerId player}
            <table .matchbox .data>
              <tr>
                <th .colgroup colspan=2>Δ
                <th .colgroup colspan=3>Adjusted Opponent
                <th .colgroup colspan=2>
              <tr>
                <th>μ
                <th>σ
                <th>μ
                <th>σ
                <th>Name
                <th>W
                <th>L
              $forall (i,(opponentId,summary)) <- itoList $ Map.toList $ view summaryMatches summ
                 <tr :odd i:.alt>
                  <td .delta>^{formatDelta $ view summaryMeanChange   summary}
                  <td .delta>^{formatDelta $ view summaryStddevChange summary}
                  <td .quiet .rating>#{showRound $ lawMean   $ view summaryAdjustedLaw summary}
                  <td .quiet .rating>#{showRound $ lawStddev $ view summaryAdjustedLaw summary}
                  <td .opponent>
                    $with Just opponent <- view (at opponentId) players
                      ^{playerLink opponentId opponent}
                  $with o <- view summaryOutcome summary
                    $with (w,l) <- (view outcomeWins o, view outcomeLosses o)
                      <td :isZero w:.quiet .outcome>#{w}
                      <td :isZero l:.quiet .outcome>#{l}
              <tr>
                <th colspan=2>Σ
                <th colspan=3>Final
                <th colspan=2>Σ
              <tr>
                $with (initial,final) <- (view summaryInitialLaw summ, view summaryFinalLaw summ)
                  <td .delta>^{formatDelta $ lawMean   final - lawMean initial}
                  <td .delta>^{formatDelta $ lawStddev final - lawStddev initial}
                  <td .rating>#{showRound $ lawMean final}
                  <td .rating>#{showRound $ lawStddev final}
                  <td .opponent>^{playerLink playerId player}
                  $with ws <- countWins summ
                    <td .outcome :isZero ws:.quiet>#{ws}
                  $with ls <- countLosses summ
                    <td .outcome :isZero ls:.quiet>#{ls}
|]

isZero :: Int -> Bool
isZero z = z == 0

formatTournamentTitle :: Event -> String
formatTournamentTitle event
  = "Tournament Results - "
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
