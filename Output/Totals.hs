{-# LANGUAGE QuasiQuotes #-}
module Output.Totals where

import Control.Lens
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Ratio
import Data.Time.Calendar (Day)
import NewTTRS.Law
import Text.Hamlet (Html, shamlet)
import qualified Data.Map as Map

import DataStore
import Output.Common
import Player

tournamentColumns :: Int
tournamentColumns = 2


totalsHtml ::
  Map PlayerId Player ->
  Map (PlayerId, PlayerId) Int ->
  Map PlayerId (Day, Law) ->
  Html
totalsHtml players matches laws = [shamlet|
$doctype 5
 <html>
  <head>
    ^{metaTags}
    <title>#{title}
    <link rel=stylesheet type=text/css href=/static/common.css>
    <link rel=stylesheet type=text/css href=/static/results.css>
  <body>
    ^{navigationLinks}
    <h1>#{title}
    ^{matchesMatrix players matches sortedPlayerIds}

    $forall playerId <- sortedPlayerIds
     $with Just player <- Map.lookup playerId players
      <div .resultbox>
        <span .playername>
          ^{playerLink playerId player}
        <table .data>
          <tr>
            <th>Opponent
            <th>W
            <th>L
            <th>Σ
            <th>%
            <th>Share
          $forall opponentId <- sortedPlayerIds
           $with Just opponent <- Map.lookup opponentId players
            $with wins <- look playerId opponentId
              $with losses <- look opponentId playerId
                $if isInteresting wins losses
                  <tr>
                    <td>
                      ^{playerLink opponentId opponent}
                    <td .num>#{wins}
                    <td .num>#{losses}
                    <td .num>#{plus wins losses}
                    <td .num>#{ratio wins losses}
                    <td .num>#{ratio (plus wins losses) (played opponentId)}
                $else
                  <tr>
                    <td>
                      ^{playerLink opponentId opponent}
                    <td>
                    <td>
                    <td>
                    <td>
                    <td>
          <tr>
            <th>Totals
            $with totalWins <- countWins playerId
              $with totalLosses <- countLosses playerId
                <td .num>#{totalWins}
                <td .num>#{totalLosses}
                <td .num>#{plus totalWins totalLosses}
                <td .num>#{ratio totalWins totalLosses}
                <td>N/A
|]
  where
  title = "Totals"
  look winner loser = view (at (winner,loser) . non 0) matches
  isInteresting wins losses = wins > 0 || losses > 0

  plus = (+)

  sortedPlayerIds :: [PlayerId]
  sortedPlayerIds
    = map fst
    $ sortBy (flip (comparing (lawScore . snd . snd)))
    $ Map.toList laws

  ratio wins losses = show (round (wins % (wins + losses) * 100) :: Integer)

  played playerId = fromJust $ Map.lookup playerId playedMap

  playedMap = Map.fromListWith (+)
               [ (playerId, n)
               | ((w,l),n) <- Map.toList matches
               , playerId <- [w,l]
               ]

  countWins   p = sum [n | ((w,_),n) <- Map.toList matches, p == w]
  countLosses p = sum [n | ((_,l),n) <- Map.toList matches, p == l]

matchesMatrix :: Map PlayerId Player -> Map (PlayerId, PlayerId) Int -> [PlayerId] -> Html
matchesMatrix players matches sortedPlayerIds = [shamlet|

<table .data>
  $forall playerId <- sortedPlayerIds
    <tr>
      $with Just player <- Map.lookup playerId players
        <th>^{playerLink playerId player}
        $forall opponentId <- sortedPlayerIds
          $if playerId == opponentId
            <th>
          $else
            <td .num>
              $maybe n <- Map.lookup (playerId, opponentId) matches
                #{n}
|]
