{-# LANGUAGE QuasiQuotes #-}
module Output.Player where

import Text.Hamlet
import Control.Lens
import qualified Data.Map as Map
import Text.Blaze.Html (preEscapedToHtml)

import Event
import Output.Formatting
import Output.Common
import DataStore
import Player
import Law

import Snap.Snaplet.SqliteSimple

playerPage :: (Functor m, HasSqlite m) => PlayerId -> m Html
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
    <script language=javascript>#{preEscapedToHtml $ graphScript laws}
  <body>
    ^{navigationLinks}
    <h1>Report for #{view playerName player}
    <table .data>
       <tr>
         <th colspan=2>Event
         <th colspan=3>Rating
       <tr>
         <th>Name
         <th>Date
         <th>μ
         <th>σ
         <th>Graph
       $forall (i,(eventId,(event,law))) <- itoList rows
         <tr :odd i:.alt>
           <td>#{view eventName event}
           <td>#{formatShortDay $ view eventDay event}
           <td .num>#{showRound $ lawMean law}
           <td .num>#{showRound $ lawStddev law}
           <td>
             <div #graph#{i} .bargraph>
|]


