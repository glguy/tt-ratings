{-# LANGUAGE QuasiQuotes #-}

module Output.Events where

import Control.Lens
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Text.Hamlet
import qualified Data.Map as Map

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
      $forall (i,(eventId,event)) <- itoList sorted
        <tr :odd i:.alt>
          <td>
            <a href=#{mkEventUrl eventId}>
              #{formatLongDay $ view eventDay event}
|]
  where
  sorted = sortBy cmp $ Map.toList events
  cmp    = flip $ comparing $ view $ _2 . eventDay
