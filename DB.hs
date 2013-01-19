{-# LANGUAGE RecordWildCards #-}
module DB where

import Control.Monad(unless)
import Control.Exception as X (throwIO,catch,SomeException(..))
import Data.List((\\),find)
import Data.Char(isDigit)
import Data.Time
import Data.Maybe(catMaybes)
import System.FilePath
import System.Directory
import System.IO(hClose,hPrint,openTempFile)
import Text.Read(readMaybe)

data Match a = Match { time :: String, winner :: a, loser :: a }
              deriving (Read, Show)

data Player  = Player { playerId :: Int, playerName :: String }
              deriving (Read, Show)


matchDir :: FilePath
matchDir = "matches"

playerFile :: FilePath
playerFile = "players.hs"

tournamentDir :: Day -> FilePath
tournamentDir d = matchDir </> show d

getMatches :: Day -> IO [(String,Match Int)]
getMatches d =
  do let dir = tournamentDir d
     createDirectoryIfMissing True dir
     ds <- getDirectoryContents dir
     catMaybes `fmap` mapM (getMatch d) (ds \\ [".",".."])

getMatch :: Day -> String -> IO (Maybe (String, Match Int))
getMatch d f =
  do let file = tournamentDir d </> f
     txt <- readFile file
     case readMaybe txt of
       Nothing -> do putStrLn $ "Failed to parse match:\n" ++ txt
                     return Nothing
       Just x  -> return (Just (f,x))
  `X.catch` \e@(SomeException _) -> do print e
                                       return Nothing

getLocalTime :: IO LocalTime
getLocalTime =
  do tz <- getCurrentTimeZone
     utcToLocalTime tz `fmap` getCurrentTime

saveMatch :: String -> String -> IO ()
saveMatch w l =
  do t   <- getLocalTime
     let d = localDay t
         dir = tournamentDir d
         ti = localTimeOfDay t
         time = show (ti { todSec = fromInteger $ round (todSec ti) })
     ps     <- getPlayers
     winner <- findPlayer ps w
     loser  <- findPlayer ps l
     createDirectoryIfMissing True dir
     (_,h) <- openTempFile dir "match.hs"
     hPrint h Match { .. }
     hClose h

delMatch :: Day -> String -> IO ()
delMatch d f =
  do unless (ok f) $ err "Does not look like a match file name."
     mb <- getMatch d f
     case mb of
       Just _ -> removeFile (tournamentDir d </> f)
       Nothing -> err "Does not look like a match file"
  where
  ok x = takeExtension x == ".hs" &&
         case break isDigit (takeBaseName x) of
           ("match",bs) -> all isDigit bs
           _            -> False


getPlayers :: IO [Player]
getPlayers =
  do exists <- doesFileExist playerFile
     unless exists $ writeFile playerFile "[]"
     txt <- readFile playerFile
     case readMaybe txt of
       Nothing -> err "I can't understand the player file."
       Just xs -> return xs

findPlayer :: [Player] -> String -> IO Int
findPlayer ps x =
  case find ((x ==) . playerName) ps of
    Nothing -> err "I don't know this player."
    Just n  -> return (playerId n)

findPlayerById :: [Player] -> Int -> IO String
findPlayerById ps n =
  case find ((n ==) . playerId) ps of
    Just p  -> return (playerName p)
    Nothing -> err "I don't know this player id"

resolveMatch :: [Player] -> Match Int -> IO (Match String)
resolveMatch ps Match { .. } =
  do winner <- findPlayerById ps winner
     loser  <- findPlayerById ps loser
     return Match { .. }

err :: String -> IO a
err x = throwIO $ userError x
