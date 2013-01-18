module Main where

import LawSerialization
import Data.Foldable hiding (sum)
import Control.Lens
import Data.Map (Map)
import Data.Maybe
import Data.Time.Calendar (fromGregorian)
import Law
import System.IO
import Text.XHtml
import Tournament
import TournamentSummaryHtml
import qualified Data.Map as Map
import Data.Array.Unboxed

-- Test data based on http://ratingscentral.com/EventDetail.php?EventID=12091

type Name = String

testTournament :: [Match Name]
testTournament = map (uncurry Match)
  [ ("Beck", "Pike")
  , ("Diatchki", "Ochs")
  , ("Diatchki", "Dugan")
  , ("Estrada", "Schultz")
  , ("Estrada", "Beck")
  , ("Estrada", "Jackson")
  , ("Ghosh", "Hussain")
  , ("Graugnard", "Sullivan")
  , ("Graugnard", "Dugan")
  , ("Graugnard", "Hussain")
  , ("Graves", "Dugan")
  , ("Gunsul", "Sullivan")
  , ("Gunsul", "Hussain")
  , ("Holloway", "Pike")
  , ("Holloway", "Steinberg")
  , ("Holloway", "Rodriquez")
  , ("Holloway", "Rodriquez")
  , ("Hope", "Beck")
  , ("Hope", "Jackson")
  , ("Jackson", "Krekos")
  , ("Krekos", "Holloway")
  , ("Krekos", "Holloway")
  , ("Krekos", "Xiao")
  , ("Margolis", "Ochs")
  , ("Margolis", "Dugan")
  , ("Nimmer", "Graugnard")
  , ("Nimmer", "Graves")
  , ("Nimmer", "Ghosh")
  , ("Nimmer", "Gunsul")
  , ("Ochs", "Graves")
  , ("Pike", "Jackson")
  , ("Rodriquez", "Xiao")
  , ("Scalpone", "Margolis")
  , ("Scalpone", "Diatchki")
  , ("Scalpone", "Graves")
  , ("Schultz", "Hope")
  , ("Schultz", "Pike")
  , ("Steinberg", "Holloway")
  , ("Steinberg", "Xiao")
  , ("Sullivan", "Ghosh")
  ]

testLaws :: Map Name Law
testLaws = Map.fromList
  [("Beck", normalLaw 741 26)
  ,("Diatchki", normalLaw 737 30)
  ,("Dugan", normalLaw 515 58)
  ,("Estrada", normalLaw 890 39)
  ,("Ghosh", normalLaw 561 50)
  ,("Graugnard", normalLaw 642 35)
  ,("Graves", normalLaw 649 36)
  ,("Gunsul", normalLaw 473 42)
  ,("Holloway", normalLaw 482 43)
  ,("Hope", normalLaw 741 34)
  ,("Hussain", normalLaw 381 103)
  ,("Jackson", normalLaw 610 50)
  ,("Krekos", normalLaw 600 36)
  ,("Margolis", normalLaw 820 35)
  ,("Nimmer", normalLaw 590 36)
  ,("Ochs", normalLaw 715 24)
  ,("Pike", normalLaw 646 45)
  ,("Rodriquez", normalLaw 461 39)
  ,("Scalpone", normalLaw 831 29)
  ,("Schultz", normalLaw 784 34)
  ,("Steinberg", normalLaw 480 43)
  ,("Sullivan", normalLaw 555 37)
  ,("Xiao", normalLaw 429 53)
  ]

showMatchSummary :: Name -> MatchSummary -> String
showMatchSummary name matchSummary = unlines
  [ "Opponent: " ++ name
  , "Rating: " ++ formatLaw (summaryAdjustedLaw matchSummary)
  , "Change: " ++ show (round (summaryPointChange matchSummary) :: Integer)
  , "Outcome: " ++ formatOutcome (summaryOutcome matchSummary)
  ]

main :: IO ()
main = do
  let results = updateLawsForTournament testTournament testLaws
  ifor_ results $ \who (newLaw, summaries) ->
     do putStrLn "=================="
        putStrLn who
        let oldLaw = fromMaybe defaultLaw $ view (at who) testLaws
        putStrLn $ formatLawChange oldLaw newLaw
        putStrLn ""
        ifor_ summaries $ \i summary ->
          putStrLn $ showMatchSummary i summary

  let tournamentDay = fromGregorian 2013 1 20

  writeFile "ratings.html"    $ renderHtml $ ratingsHtml $ fmap ((,)tournamentDay) testLaws
  writeFile "tournament.html" $ renderHtml $ tournamentHtml testLaws tournamentDay results

  writeFile "ratings.csv" $ serializeLaws
     $ Map.fromList $ itoList $ map (\x -> (tournamentDay, fst x))$ Map.elems results


checkLaw law = lawError law (normalLaw (lawMean law)(lawStddev law))

lawError a b = sum (zipWith (\x y -> abs (x-y)) (elems (lawRaw a)) (elems (lawRaw b)))
