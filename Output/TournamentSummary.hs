{-# LANGUAGE QuasiQuotes #-}
module Output.TournamentSummary where

import Control.Lens ( itoList, folded, sumOf, view, At(at) )
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy, elemIndex)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day)
import Data.Ord (comparing)
import NewTTRS.Law ( Law(lawStddev, lawMean), lawScore, chanceToWin )
import NewTTRS.Outcome ( outcomeWins, outcomeLosses )
import NewTTRS.Tournament ( PlayerSummary, summaryInitialLaw, summaryFinalLaw, degradeLaw, summaryMatches, summaryOutcome, summaryAdjustedLaw, summaryMeanChange, summaryStddevChange )
import Text.Hamlet (Html, shamlet)
import qualified Data.Map as Map

import DataStore ( PlayerId )
import Event ( Event, eventDay )
import Output.Common ( playerLink, metaTags, navigationLinks, graphInclude, formatDelta )
import Output.Formatting ( formatLongDay, showRound )
import Player ( Player )

tournamentColumns :: Int
tournamentColumns = 2

data SummaryTableRow = SummaryTableRow
  { rowPlayerId :: PlayerId
  , rowPlayer :: Player
  , rowRankChange :: Int
  , rowInitialLaw :: Law
  , rowFinalLaw :: Law
  }

buildSummaryRows ::
  Map PlayerId Player ->
  Map PlayerId (Day, Law) ->
  Map PlayerId (PlayerSummary PlayerId) ->
  Day ->
  [SummaryTableRow]
buildSummaryRows players laws changes today = rows
  where
  rows          = sortBy cmpRow $ map mkSummaryRow $ Map.toList beforeAndAfter
  cmpRow        = flip (comparing (lawScore . rowFinalLaw))

  -- laws as they were before and after the event, for players who didn't
  -- play the law will be the same before and after
  beforeAndAfter = fmap changeCase changes `Map.union` fmap sameCase laws
    where
    changeCase c = (view summaryInitialLaw c, view summaryFinalLaw c)
    sameCase (day,law) = (degradedLaw,degradedLaw)
      where
      degradedLaw = degradeLaw today day law

  -- Score index in these list corresponds to initial and final "rank"
  initialLaws = mkOrderedScoreList (fmap fst beforeAndAfter)
  finalLaws   = mkOrderedScoreList (fmap snd beforeAndAfter)
  mkOrderedScoreList m = sortBy (flip compare) (fmap lawScore (toList m))

  mkSummaryRow (playerId, (initialLaw,finalLaw)) =
    SummaryTableRow
    { rowPlayerId     = playerId
    , rowPlayer       = fromJust (Map.lookup playerId players)
    , rowRankChange   = fromJust $ -- All the laws are definitely in the ordered law lists
                      do finalRank   <- lawScore finalLaw   `elemIndex` finalLaws
                         initialRank <- lawScore initialLaw `elemIndex` initialLaws
                         return (initialRank - finalRank)
    , rowInitialLaw   = initialLaw
    , rowFinalLaw     = finalLaw
    }

tournamentHtml ::
  Map PlayerId Player {- ^ map of all playerids to players -} ->
  Event               {- ^ details of current event -} ->
  Map PlayerId (PlayerSummary PlayerId) {- ^ changes due to current event -} ->
  Map PlayerId (Day, Law) {- ^ map of all laws as of the current event -} ->
  Html
tournamentHtml players event results laws = [shamlet|
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
  sorted     = buildSummaryRows players laws results (view eventDay event)
  sortedLaws = map rowFinalLaw sorted
  summary = [shamlet|
    <table .summary .data>
      <tr>
        <th>
        <th .colgroup colspan=6>Δ
        <th .colgroup colspan=3>Final
      <tr>
        <th>Name
        <th colspan=2>μ
        <th colspan=2>σ
        <th colspan=2>##
        <th>μ
        <th>σ
        <th>Graph
      $forall (i,row) <- itoList sorted
        <tr :odd i:.alt>
          <td .opponent>
            ^{playerLink (rowPlayerId row) (rowPlayer row)}
            ^{formatDelta $ on (-) lawMean   (rowFinalLaw row) (rowInitialLaw row)}
            ^{formatDelta $ on (-) lawStddev (rowFinalLaw row) (rowInitialLaw row)}
            ^{formatDelta $ fromIntegral $ rowRankChange row}
          <td .num .rating>#{showRound $ lawMean   $ rowFinalLaw row}
          <td .num .rating>#{showRound $ lawStddev $ rowFinalLaw row}
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
                <th .colgroup colspan=4>Δ
                <th .colgroup colspan=3>Adjusted Opponent
                <th .colgroup colspan=2>
                <th .colgroup colspan=2>%
              <tr>
                <th colspan=2>μ
                <th colspan=2>σ
                <th>μ
                <th>σ
                <th>Name
                <th>W
                <th>L
                <th>Act
                <th>Exp
              $forall (i,(opponentId,summary)) <- itoList $ Map.toList $ view summaryMatches summ
                 <tr :odd i:.alt>
                  ^{formatDelta $ view summaryMeanChange   summary}
                  ^{formatDelta $ view summaryStddevChange summary}
                  <td .quiet .rating>#{showRound $ lawMean   $ view summaryAdjustedLaw summary}
                  <td .quiet .rating>#{showRound $ lawStddev $ view summaryAdjustedLaw summary}
                  <td .opponent>
                    $with Just opponent <- view (at opponentId) players
                      ^{playerLink opponentId opponent}
                  $with o <- view summaryOutcome summary
                    $with (w,l) <- (view outcomeWins o, view outcomeLosses o)
                      <td :isZero w:.quiet .outcome>#{w}
                      <td :isZero l:.quiet .outcome>#{l}
                      <td .quiet .outcome>#{showRound $ pct w $ w + l}
                  <td .quiet .rating>#{showRound $ times 100 $ chanceToWin (view summaryInitialLaw summ) (view summaryAdjustedLaw summary)}
              <tr>
                <th colspan=4>Σ
                <th colspan=3>Final
                <th colspan=4>Σ
              <tr>
                $with (initial,final) <- (view summaryInitialLaw summ, view summaryFinalLaw summ)
                  ^{formatDelta $ lawMean   final - lawMean initial}
                  ^{formatDelta $ lawStddev final - lawStddev initial}
                  <td .rating>#{showRound $ lawMean final}
                  <td .rating>#{showRound $ lawStddev final}
                  <td .opponent>^{playerLink playerId player}
                  $with (w,l) <- (countWins summ, countLosses summ)
                    <td .outcome :isZero w:.quiet>#{w}
                    <td .outcome :isZero l:.quiet>#{l}
                    <td .quiet>#{showRound $ pct w $ w + l}
                  <td .quiet>-
|]

isZero :: Int -> Bool
isZero z = z == 0

pct :: Int -> Int -> Double
pct x y = 100 * fromIntegral x / fromIntegral y

times :: Num a => a -> a -> a
times x y = x * y

formatTournamentTitle :: Event -> String
formatTournamentTitle event
  = "Tournament Results - "
 ++ formatLongDay (view eventDay event)
