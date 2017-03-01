module Genetic where

import System.Random
import GHC.Exts

type Duration = Int -- Minutes
type Position = Int
type Individual = [ReviewSchedule]
type Population = [Individual]

newtype ReviewSchedule = ReviewSchedule [Duration] deriving Show


(|>) f x = x f


swapTwo :: Int -> Int -> [a] -> [a]
swapTwo i j xs = zipWith (\x y -> 
    if x == i then xs !! j
    else if x == j then xs !! i
    else y) [0..] xs


instance Monoid ReviewSchedule where
  mempty = ReviewSchedule [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  mappend (ReviewSchedule a) (ReviewSchedule b) = ReviewSchedule $ zipWith (+) a b


fitness :: Individual -> Float
fitness xs = sum $ map (\x -> (fromIntegral x - avg)^2) values
  where 
   ReviewSchedule values = mconcat xs
   avg = fromIntegral(sum values) / 12.0


rowEvolver :: (Int, Int) -> ReviewSchedule -> ReviewSchedule
rowEvolver (i, j) (ReviewSchedule values) = ReviewSchedule $ swapTwo i j values


listEvolver :: (Int, Int, Int) -> Individual -> Individual
listEvolver (row, i, j) individual
  | row == 0         = rowEvolver (i, j) x : xs
  | row `elem` range = take (row-1) individual ++ listEvolver (0, i, j) (drop (row-1) individual)
  | otherwise        = individual
  where
    size = length individual
    range = [0..size-1]
    (x:xs) = individual


nextGeneration :: RandomGen g => (Individual, g) -> (Population, g)
nextGeneration (individual, generator) =
  (map ($ individual) evolvers, snd $ next generator)
  where
    aggro     = 200
    size      = length individual
    positions = cycle [0..size-1]
    (<$>)     = map
    (<*>)     = zipWith ($)
    list      = randomRs (0 :: Int,11) generator
    xs = take (aggro*size) list
    ys = drop (aggro*size) list |> take (aggro*size)
    -- zipWith (,,) 3 lists
    tuples = (,,) <$> positions <*> xs <*> ys
    evolvers = map listEvolver tuples

    
selectFittest :: Population -> Individual
selectFittest population =
  sortWith fitness population |> head


evolve :: RandomGen g => (Individual, g) -> (Individual, g)
evolve (individual, generator) =
  (selectFittest population, gg)
  where
    (population, gg) = nextGeneration(individual, generator)


run :: RandomGen g => Int -> (Individual, g) -> (Individual, g)
run 0                   (individual, generator) = (individual, generator)
run numberOfGenerations (individual, generator) =
  if new_fitness < old_fitness then
    run (numberOfGenerations-1) $ (evolved, gg)
  else
    (individual, generator)
  where
     old_fitness   = fitness individual
     (evolved, gg) = evolve (individual, generator)
     new_fitness   = fitness evolved
