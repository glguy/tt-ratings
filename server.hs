{-# LANGUAGE RecordWildCards, PatternGuards, QuasiQuotes #-}
import Control.Applicative
import Control.Exception
import Control.Lens
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time
import Data.Time
import Data.Time.Calendar (Day)
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import System.Locale
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamlet, Html)
import Text.Read(readMaybe)
import qualified Data.Text as Text

import Formatting
import Player
import Match
import DataStore

main :: IO ()
main = serverWith
  Config
    { srvLog = quietLogger
    , srvHost = "0.0.0.0"
    , srvPort = 8000
    }
    $ \_ URL { .. } _ ->
  case url_path of
    "match" | Just w <- lookup "winner" url_params
            , Just l <- lookup "loser"  url_params ->
             do saveMatch w l
                putStrLn $ "Saving " ++ show (w,l)
                return good

    "delete" | Just mid <- fmap MatchId . readMaybe =<< lookup "matchid" url_params ->
             do match <- getMatchById mid
                deleteMatchById mid
                putStrLn $ "Deleted " ++ show match
                return good

{-
    "exportmatches" ->
      do ms <- exportMatches
         return $ ok $ show $ over (mapped . _1) show ms
-}

    _ -> ok . renderHtml <$> mainPage
  `catch` \(SomeException e) -> return (bad (show e))

  where
  ok body = Response { rspCode   = (2,0,0)
                     , rspReason = "OK"
                     , rspHeaders = hdrs
                     , rspBody   = body }

  hdrs = [ mkHeader HdrConnection "close"]

  good = Response { rspCode = (3,0,2)
                  , rspReason = "Found"
                  , rspHeaders = hdrs ++ [ mkHeader HdrLocation "/" ]
                  , rspBody = "OK"
                  }


  bad e = Response { rspCode = (5,0,0)
                   , rspReason = "It didn't work"
                   , rspHeaders = hdrs
                   , rspBody = e
                   }

mainPage :: IO Html
mainPage =
  do t   <- zonedTimeToLocalTime <$> getZonedTime
     ms  <- getMatchesForDay (localDay t)
     ps  <- getPlayerList
     tz  <- getCurrentTimeZone
     thePage ps $ formatMatches tz (localDay t) ms

saveMatch :: String -> String -> IO MatchId
saveMatch winner loser = do
  Just eventId <- getCurrentEventId
  _matchTime   <- getCurrentTime
  Just _matchWinner <- getPlayerIdByName $ Text.pack winner
  Just _matchLoser  <- getPlayerIdByName $ Text.pack loser
  addMatchToEvent Match{..} eventId

--------------------------------------------------------------------------------

formatMatch :: TimeZone -> Day -> Int -> MatchId -> Match Player -> Html
formatMatch tz d i (MatchId mid) match = [shamlet|
  <tr :odd i:.alt>
    <td>#{t}
    <td>#{w}
    <td>#{l}
    <td>
      <a .delete href=#{exportURL delUrl}>delete
|]
  where
  t = view (matchTime   . to (formatTime defaultTimeLocale "%X" . utcToLocalTime tz)) match
  w = view (matchWinner . playerName) match
  l = view (matchLoser  . playerName) match
  delUrl = URL { url_type   = HostRelative
               , url_path   = "delete"
               , url_params = [("matchid", show mid)]
               }

formatMatches :: TimeZone -> Day -> [(MatchId, Match Player)] -> Html
formatMatches tz d xs = [shamlet|
<h2>Matches for #{formatLongDay d}
  <table>
    <tr>
      <th>Time
      <th>Winner
      <th>Loser
      <th>Actions
    $forall (i,(fn,m)) <- itoList $ sortBy (flip byTime) xs
      ^{formatMatch tz d i fn m}
|]
  where
  byTime = comparing (view matchTime . snd)

thePage :: [Player] -> Html -> IO Html
thePage ps table =
  do css <- readFile "style.css"
     return [shamlet|
<html lang=en>
  <head>
    <meta charset="UTF-8" />
    <meta name="google" content="notranslate">
    <meta http-equiv="Content-Language" content="en" />
    <title>Ping Pong Results
    <style>#{css}
  <body>
    <div .entry>
      <form action="/match" method=GET>
        <label for=winner>Winner:
        <input autocomplete=off list=players name=winner #winner>
        <label for=loser>Loser:
        <input autocomplete=off list=players name=loser  #loser>
        <datalist #players>
          $forall p <- ps
            <option value=#{view playerName p}>
        <input type=submit #submit value=Record>
    ^{table}
|]
