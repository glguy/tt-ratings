{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Output.Common where

import Control.Lens
import Data.Text (Text)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet
import qualified Data.Text as Text

import DataStore
import Law
import Player

graphStddevCutoff :: Double
graphStddevCutoff = 150

--------------------------------------------------------------------------------
-- URL generators

mkPlayerUrl :: PlayerId -> Text
mkPlayerUrl (PlayerId i) = "/player/" `Text.append` Text.pack (show i)

mkEventUrl :: EventId -> Text
mkEventUrl (EventId i) = "/event/" `Text.append` Text.pack (show i)

--------------------------------------------------------------------------------

playerLink :: PlayerId -> Player -> Html
playerLink playerId player = [shamlet|<a href=#{mkPlayerUrl playerId}>#{view playerName player}|]

--------------------------------------------------------------------------------

metaTags :: Html
metaTags = [shamlet|
  <meta charset="UTF-8" />
  <meta name="google" content="notranslate">
  <meta http-equiv="Content-Language" content="en" />
  <link rel="shortcut icon" href="/favicon.ico" />
|]

navigationLinks :: Html
navigationLinks = [shamlet|
  <div #navigation>
    <ul>
      <li>
        <a href="/">Match Entry
      <li>
        <a href="/players">Players
      <li>
        <a href="/event/latest">Latest Event
      <li>
        <a href="/events">Events
      <li>
        <a href="/static/graph.html">Curves
|]

graphInclude :: [Law] -> Html
graphInclude laws = [shamlet|
    <script language=javascript>#{preEscapedToHtml $ graphScript laws}
|]

graphScript :: [Law] -> String
graphScript laws
  = unlines
  $ "$(document).ready(function drawGraphs() {"
  : options
  ++ imap drawOne laws
 ++ ["});"]

  where
  tightEnough law = lawStddev law < graphStddevCutoff

  tightLaws
    | any tightEnough laws = filter tightEnough laws
    | otherwise            = laws

  minVal = minimum $ 3600 : map mkLo tightLaws
  maxVal = maximum $    0 : map mkHi tightLaws
  mkLo    law = lawMean law - 2 * lawStddev law
  mkLoMid law = lawMean law -     lawStddev law
  mkHiMid law = lawMean law +     lawStddev law
  mkHi    law = lawMean law + 2 * lawStddev law

  drawOne i law = "$.plot($(\"#graph"++show i++"\"), " ++ show [mkSeries law] ++ ", options);"

  mkSeries law = [[x,0] | x <- [mkLo law, mkLoMid law, lawMean law, mkHiMid law, mkHi law]]

  options =
        [ "function vertLine(ctx, x, y, radius, shadow) {"
        , "     ctx.moveTo(x, y - radius);"
        , "     ctx.lineTo(x, y + radius);"
        , "}"
        , "var options = {"
        ," xaxis: { min: " ++ show minVal ++ ", max: " ++ show maxVal ++  " , show: false },"
        ," yaxis: { show: false },"
        ," margin: { top: 0, left: 0, right: 0, bottom: 0 },"
        ," lines: { show: true },"
        ," points: { show: true, symbol: vertLine },"
        ," colors: [\"red\"]"
        ,"};"
        ]
