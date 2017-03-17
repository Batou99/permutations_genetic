module Main where

import Control.Monad.Random
import Data.Monoid
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
  Spec [8, 2, 2] [15, 20, 30]
  ]

runSeries :: (MonadRandom m) => Int -> Int -> m [Individual] -> m [Individual]
runSeries 0 amount population = runTournament amount population
runSeries n amount population =
  runSeries (n-1) amount (runTournament amount population)

main :: IO ()
main = do
  generator <- getStdGen
  let individual = fromSpecs specs
  print $ fitness individual
  
  let population = evalRandIO (nextGeneration 200 individual)
  parents <- runSeries 1000 200 population
  
  let parent = head parents
  print $ fitness parent
  print $ mconcat parent
  
