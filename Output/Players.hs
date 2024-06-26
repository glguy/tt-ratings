{-# LANGUAGE QuasiQuotes #-}
module Output.Players where

import Control.Lens ( itoList, view, Field3(_3) )
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Time.Calendar ( Day )
import NewTTRS.Law ( Law(lawStddev, lawMean), lawScore )
import Text.Hamlet (Html, shamlet)
import qualified Data.Map as Map

import DataStore ( PlayerId )
import Output.Common ( playerLink, metaTags, navigationLinks, graphInclude )
import Output.Formatting ( formatShortDay, formatLongDay, showRound )
import Player ( Player )

playersHtml :: Day -> Map PlayerId (Player,Day,Law) -> Html
playersHtml day laws = [shamlet|
$doctype 5
<html>
  <head>
    ^{metaTags}
    <title>#{title}
    <link rel=stylesheet href=/static/common.css>
    <link rel=stylesheet href=/static/ratings.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
    <script language=javascript src=/static/jquery.flot.js>
    ^{graphInclude $ map rowLaw rows}
  <body>
    ^{navigationLinks}
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
        $forall (i,(playerId,(player,lastday,law))) <- itoList rows
          <tr :odd i:.alt>
            <td .str>
               ^{playerLink playerId player}
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
  byScore     = flip (comparing (lawScore . view _3 . snd))
  rows        = sortBy byScore $ Map.toList laws
  rowLaw (_,(_,_,law)) = law
