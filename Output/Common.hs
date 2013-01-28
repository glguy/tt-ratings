{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Output.Common where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Hamlet
import Control.Lens

import DataStore
import Law

--------------------------------------------------------------------------------
-- URL generators

mkPlayerUrl :: PlayerId -> Text
mkPlayerUrl (PlayerId i) = "/player/" `Text.append` Text.pack (show i)

mkEventUrl :: EventId -> Text
mkEventUrl (EventId i) = "/event/" `Text.append` Text.pack (show i)

--------------------------------------------------------------------------------

metaTags :: Html
metaTags = [shamlet|
  <meta charset="UTF-8" />
  <meta name="google" content="notranslate">
  <meta http-equiv="Content-Language" content="en" />
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

graphScript :: [Law] -> String
graphScript laws
  = unlines
  $ "$(document).ready(function drawGraphs() {"
  : options
  ++ imap drawOne laws
 ++ ["})"]
  where
  drawOne i law =
    "$.plot($(\"#graph"++show i++"\"), [" ++ show dat ++ "], options);"
    where
    dat = map (\x -> [x,0]) [mkLo law, mkLoMid law, lawMean law, mkHiMid law, mkHi law]
  mkLo law = lawMean law - 2 * lawStddev law
  mkLoMid law = lawMean law - lawStddev law
  mkHiMid law = lawMean law + lawStddev law
  mkHi law = lawMean law + 2 * lawStddev law
  minVal = minimum $ map mkLo laws
  maxVal = maximum $ map mkHi laws
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

