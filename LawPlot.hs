module LawPlot where

import Law
import Graphics.Gnuplot.Simple
import Data.Array.Unboxed

plotLaw :: FilePath -> Law -> IO ()
plotLaw fn law = plotList [PNG fn] xs
  where
  xs = zip omega $ elems $ lawRaw law

plotLaws :: FilePath -> [Law] -> IO ()
plotLaws fn laws = plotLists [Key Nothing, PNG fn] (map f laws)
  where
  f law = zip omega $ elems $ lawRaw law
