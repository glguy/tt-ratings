{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import DataStore
import Snap.Snaplet.SqliteSimple
import Database.SQLite.Simple
import qualified Data.Map as Map
import Data.List
import Control.Lens
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Concurrent.MVar

import Event
import NewTTRS.Law
import NewTTRS.Tournament

newtype M a = M (ReaderT Sqlite IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatchIO)

instance HasSqlite M where
  getSqliteState = M ask

runM :: M a -> IO a
runM (M m) = withConnection "pingpong.db" $ \conn ->
  do mvar <- newMVar conn
     runReaderT m (Sqlite mvar)

main = runM $ do
  events <- getEvents
  results <- fmap Map.elems $
    ifor events $ \eventId eventInfo ->
      do laws <- getLawsForEvent False eventId
         let today = eventInfo^.eventDay
         --let degradedLaws = fmap (\(day,law) -> degradeLaw today day law) laws
         --let lawSummaries = fmap (\law -> (lawMean law, lawStddev law)) degradedLaws
         let lawSummaries = fmap (\(_,law) -> (lawMean law, lawStddev law)) laws
         return lawSummaries
  let allPlayers = nub $ concatMap Map.keys results

  let processPlayer lawMap player
        = case Map.lookup player lawMap of
            Nothing -> 1800
            Just (mean,_) -> mean

  let processEvent lawMap = map (processPlayer lawMap) allPlayers
  let allMeans = map processEvent results
  let byPlayer = transpose allMeans

  forM_ byPlayer $ \playerData ->
    liftIO $ putStrLn $ intercalate "," $ map (show . truncate) playerData
