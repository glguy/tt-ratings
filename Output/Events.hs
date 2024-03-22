{-# LANGUAGE QuasiQuotes #-}

module Output.Events where

import Control.Lens ( itoList, view, Field2(_2) )
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Text.Hamlet ( Html, shamlet )
import qualified Data.Map as Map

import DataStore (EventId)
import Event ( Event, eventDay )
import Output.Formatting ( formatLongDay )
import Output.Common ( mkEventUrl, metaTags, navigationLinks )

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
