{-# LANGUAGE QuasiQuotes #-}

module Output.Events where

import Text.Hamlet
import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)

import DataStore (EventId)
import Event
import Output.Formatting
import Output.Common

eventsPage :: Map EventId Event -> Html
eventsPage events = [shamlet|
$doctype 5
<html>
  <head>
    ^{metaTags}
    <title>Events
    <link rel=stylesheet href=/static/common.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
  <body>
    ^{navigationLinks}
    <h1>Events
    <table .data>
      <tr>
        <th>Day
      $forall (i,(eventId,event)) <- itoList $ Map.toList events
        <tr :odd i:.alt>
          <td>
            <a href=#{mkEventUrl eventId}>
              #{formatShortDay $ view eventDay event}
|]
