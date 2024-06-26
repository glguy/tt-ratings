{-# LANGUAGE QuasiQuotes #-}
module Output.Player where

import Control.Lens
import NewTTRS.Law
import Text.Hamlet
import qualified Data.Map as Map

import Event ( eventDay )
import Output.Formatting ( formatShortDay, showRound )
import Output.Common ( mkEventUrl, metaTags, navigationLinks, graphInclude )
import DataStore ( PlayerId, getPlayerById, getLawsForPlayer )
import Player ( playerName )

import Snap.Snaplet.SqliteSimple ( HasSqlite )

playerPage ::
  (MonadFail m, HasSqlite m) =>
  PlayerId -> m Html
playerPage playerId = do
   Just player <- getPlayerById playerId
   events <- getLawsForPlayer playerId
   let rows = reverse $ Map.toList events
   let laws = map (snd.snd) rows
   return $ [shamlet|
$doctype 5
<html>
  <head>
    ^{metaTags}
    <title>Player
    <link rel=stylesheet type=text/css href=/static/common.css>
    <link rel=stylesheet type=text/css href=/static/ratings.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
    <script language=javascript src=/static/jquery.flot.js>
    ^{graphInclude laws}
  <body>
    ^{navigationLinks}
    <h1>Report for #{view playerName player}
    <table .data>
       <tr>
         <th>Event
         <th colspan=3>Rating
       <tr>
         <th>Date
         <th>μ
         <th>σ
         <th>Graph
       $forall (i,(eventId,(event,law))) <- itoList rows
         <tr :odd i:.alt>
           <td>
             <a href=#{mkEventUrl eventId}>
               #{formatShortDay $ view eventDay event}
           <td .num>#{showRound $ lawMean law}
           <td .num>#{showRound $ lawStddev law}
           <td>
             <div #graph#{i} .bargraph>
|]


