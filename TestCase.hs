import Control.Lens
import Tournament
import Law
import qualified Data.Map as Map

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

showMatchSummary name matchSummary = unlines
  [ "Name: " ++ name
  , "Rating: " ++ formatLaw (matchSummary^.updatedLaw)
  , "Change: " ++ show (round (matchSummary^.pointChange))
  ]

formatLaw law =
  show (round mean) ++ "±" ++ show (round stddev)
  where
  (mean,stddev) = lawMeanStddev law

formatLawChange old new =
  formatLaw old
  ++ " + " ++ show (round mean2 - round mean1) ++ " = " 
  ++ formatLaw new
  where
  (mean1,_) = lawMeanStddev old
  (mean2,_) = lawMeanStddev new

main = ifor_ (updateLawsForTournament testTournament testLaws)
     $ \who (law, summaries) ->
     do putStrLn "=================="
        putStrLn who
        let Just oldlaw = testLaws ^. at who
        putStrLn $ formatLawChange oldlaw law
        ifor_ summaries $ \i summary ->
          putStrLn $ showMatchSummary i summary


