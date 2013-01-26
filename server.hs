{-# LANGUAGE RecordWildCards, PatternGuards, QuasiQuotes #-}
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import Data.Time
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Hamlet (shamlet, Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Time.Calendar (Day)
import Control.Applicative

import Text.Read(readMaybe)
import Control.Exception
import Control.Lens

import DB
import Formatting
import ExportMatches

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

    "delete" | Just dtxt <- lookup "day" url_params
             , Just d <- readMaybe dtxt
             , Just f <- lookup "match" url_params ->
             do delMatch d f
                putStrLn $ "Deleted " ++ show (d,f)
                return good

    "exportmatches" ->
      do ms <- exportMatches
         return $ ok $ show $ over (mapped . _1) show ms

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
  do t   <- getLocalTime
     ms  <- getMatches (localDay t)
     ps  <- getPlayers
     msR <- mapM (\(x,m) -> (,) x `fmap` resolveMatch ps m) ms
     thePage ps $ formatMatches (localDay t) msR

--------------------------------------------------------------------------------

formatMatch :: Day -> Int -> FilePath -> Match String -> Html
formatMatch d i f Match { .. } = [shamlet|
  <tr :odd i:.alt>
    <td>#{time}
    <td>#{winner}
    <td>#{loser}
    <td>
      <a .delete href=#{exportURL delUrl}>delete
|]
  where
  delUrl = URL { url_type   = HostRelative
               , url_path   = "delete"
               , url_params = [("day", show d), ("match",f)]
               }

formatMatches :: Day -> [(FilePath, Match String)] -> Html
formatMatches d xs = [shamlet|
<h2>Matches for #{formatLongDay d}
  <table>
    <tr>
      <th>Time
      <th>Winner
      <th>Loser
      <th>Actions
    $forall (i,(fn,m)) <- itoList $ sortBy (flip byTime) xs
      ^{formatMatch d i fn m}
|]
  where
  byTime = comparing (time . snd)

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
          $forall p <- map playerName ps
            <option value=#{p}>
        <input type=submit #submit value=Record>
    ^{table}
|]
