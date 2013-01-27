{-# LANGUAGE QuasiQuotes #-}

module Output.Events where

import Text.Hamlet
import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)

import Event
import DataStore (EventId(EventId))
import Output.Formatting
import Output.Common

eventsPage :: Map EventId (Event EventId) -> Html
eventsPage events = [shamlet|
$doctype 5
<html>
  <head>
    ^{metaTags}
    <title>Events
    <link rel=stylesheet href=static/common.css>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
  <body>
    ^{navigationLinks}
    <h1>Events
    <form action="/events" method="post">
      <div>
        <input type=submit name=action value=Open>
        <input type=submit name=action value=Close>
        <input type=submit name=action value=Delete>
      <table .data>
        <tr>
          <th>#
          <th>Name
          <th>Day
          <th>Active
          <th>Previous
       $forall (i,(eventId,event)) <- itoList $ Map.toList events
         <tr :odd i:.alt>
           <td>
             $with str <- show $ op EventId eventId
               <input #eventChoice#{i} type=radio name=eventId value=#{str}>
               <label for=eventChoice#{i}>#{str}
           <td>#{view eventName event}
           <td>#{formatShortDay $ view eventDay event}
           <td>
             $if view eventActive event
                 Open
             $else
                 Closed
           <td>
             $maybe x <- view eventPrevious event
               #{show $ op EventId x}
             $nothing
               N/A
|]
