module Main where

import System.Random
import Genetic

type Revisions = [Int]
type Durations = [Int]
data Spec = Spec Revisions Durations

fromSpec :: Spec -> ReviewSchedule
fromSpec (Spec revs durs) =
  ReviewSchedule $ concat (zipWith replicate revs durs)


fromSpecs :: [Spec] -> Individual
fromSpecs = map fromSpec


specs = [ 
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [20, 30, 50],
  Spec [6, 4, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [15, 20, 30],
  Spec [6, 4, 2] [20, 30, 50],
  Spec [8, 2, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [20, 30, 50],
  Spec [6, 4, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [15, 20, 30],
  Spec [6, 4, 2] [20, 30, 50],
  Spec [8, 2, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [20, 30, 50],
  Spec [6, 4, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [15, 20, 30],
  Spec [6, 4, 2] [20, 30, 50],
  Spec [8, 2, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [20, 30, 50],
  Spec [6, 4, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [15, 20, 30],
  Spec [6, 4, 2] [20, 30, 50],
  Spec [8, 2, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [20, 30, 50],
  Spec [6, 4, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [7, 3, 2] [15, 20, 30],
  Spec [6, 4, 2] [20, 30, 50],
  Spec [8, 2, 2] [25, 35, 55],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28],
  Spec [8, 2, 2] [15, 20, 30],
  Spec [8, 2, 2] [12, 15, 28]
  ]

main :: IO ()
main = do
  generator <- getStdGen
  let individual = fromSpecs specs
  print $ fitness individual
  let (newgen, gg) = run 200 (individual, generator)
  print $ fitness newgen
  print newgen
  print $ mconcat newgen

