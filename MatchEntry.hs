{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

#ifndef MIN_VERSION_time
#define MIN_VERSION_time(x,y,z) 1
#endif

module MatchEntry (matchEntryPage) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Control.Lens.Combinators
import Data.Time
import Text.Hamlet (shamlet, Html)
import qualified Data.Map as Map
import Data.Map (Map)
import NewTTRS.Match (Match, matchTime, matchWinner, matchLoser)
import Snap.Snaplet.SqliteSimple

import DataStore
import Player (Player, playerName)
import Output.Common (metaTags, navigationLinks)
import Output.Formatting (formatLongDay)

#if !(MIN_VERSION_time(1,5,0))
import System.Locale (defaultTimeLocale)
#endif

matchEntryPage ::
  (HasSqlite m, MonadIO m) =>
  Maybe Text {- ^ error message -} ->
  Text {- ^ initial winner text -} ->
  Text {- ^ initial wins        -} ->
  Text {- ^ initial loser  text -} ->
  Text {- ^ initial losses text -} ->
  m Html
matchEntryPage err w wins l losses =
  do now <- liftIO getZonedTime
     let today = localDay (zonedTimeToLocalTime now)
         tz    = zonedTimeZone now
     mb <- getEventIdByDay today
     ps <- getPlayers
     ms <- case mb of
             Nothing            -> return Map.empty
             Just eventId       -> getMatchesByEventId eventId
     namedMatches <- maybe (fail "unknown player id") return
                   $ (traverse . traverse) (flip Map.lookup ps) ms
     return $ thePage err w wins l losses (sort (Map.elems ps))
            $ formatMatches tz today namedMatches

thePage :: Maybe Text -> Text -> Text -> Text -> Text -> [Player] -> Html -> Html
thePage err w wins l losses ps table = [shamlet|
<html lang=en>
  <head>
    ^{metaTags}
    <title>Ping Pong Results
    <link rel=stylesheet type=text/css href=/static/common.css>
    <link rel=stylesheet type=text/css href=/static/style.css>
    <script type=text/javascript src=/static/entry.js>
  <body>
    ^{navigationLinks}
    <div .entry>
      <form action="/match" method=POST>
        <input autocomplete=off list=players name=winner #winner value=#{w}>
        won
        <input type=text .outcomeNumber autocomplete=off name=wins maxlength=1 value=#{wins} onkeyup="numberchange(this.value, 'winsPlural');">
        game#
        <span #winsPlural>s
        <br>
        <input autocomplete=off list=players name=loser  #loser value=#{l}>
        won
        <input type=text .outcomeNumber autocomplete=off name=losses maxlength=1 value=#{losses}  onkeyup="numberchange(this.value, 'lossesPlural');">
        game#
        <span #lossesPlural>s
        <br>
        <input type=submit #submit value="Save Match">
        <datalist #players>
          $forall p <- ps
            <option value=#{view playerName p}>
    $maybe errMsg <- err
      <div #errorMessage>#{errMsg}
    ^{table}
|]


formatMatch :: TimeZone -> Int -> MatchId -> Match Player -> Html
formatMatch tz i (MatchId mid) match = [shamlet|
  <tr :odd i:.alt>
    <td>#{t}
    <td>#{w}
    <td>#{l}
    <td>
      <form .deleteform action="/matchop" method=post>
        <input type=hidden name=matchId value=#{show mid}>
        <input .deletebutton type=submit name=action value="copy">
        <input .deletebutton type=submit name=action value="upset">
        <input .deletebutton type=submit name=action value="delete">
|]
  where
  t = view (matchTime   . to (formatTime defaultTimeLocale "%X" . utcToLocalTime tz)) match
  w = view (matchWinner . playerName) match
  l = view (matchLoser  . playerName) match

formatMatches :: TimeZone -> Day -> Map MatchId (Match Player) -> Html
formatMatches tz d xs
  | Map.null xs = return ()
  | otherwise = [shamlet|
<h2>Matches for #{formatLongDay d}
  <table>
    <tr>
      <th>Time
      <th>Winner
      <th>Loser
      <th>Actions
    $forall (i,(fn,m)) <- itoList $ sortBy (flip byTime) $ Map.toList xs
      ^{formatMatch tz i fn m}
|]
  where
  byTime = comparing (view matchTime . snd)
