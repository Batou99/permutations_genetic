{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-}

module Genetic where

import GHC.Exts
import Data.Monoid
import Control.Monad
import Control.Monad.Random

class Genome a where
  fitness :: a -> Float
  breed :: (MonadRandom m) => a -> a -> m a
  mutate :: (MonadRandom m) => a -> m a
  nextGeneration :: (MonadRandom m) => Int -> a -> m [a]
  nextGeneration amount parent = replicateM amount (mutate parent)
  selectParents :: (MonadRandom m) => [a] -> m [a]
  runTournament :: (MonadRandom m) => Int -> m [a] -> m [a]
  runTournament amount population = do
    x:y:xs <- population
    parent <- breed x y
    children <- nextGeneration amount parent
    selectParents children

type Duration = Int -- Minutes
type Individual = [ReviewSchedule]

newtype ReviewSchedule = ReviewSchedule [Duration] deriving Show

instance Genome Individual where
  fitness xs = sum $ map (\x -> (fromIntegral x - avg)^2) values
    where 
     ReviewSchedule values = mconcat xs
     avg = fromIntegral(sum values) / 12.0

  mutate individual = do
    row  <- getRandomR (0,(length individual)-1)
    col1 <- getRandomR (0,11)
    col2 <- getRandomR (0,11)
    return $ listEvolver (row, col1, col2) individual

  breed ind1 ind2 = mutate ind1

  selectParents xs = return $ take 2 (sortWith fitness xs)


(|>) f x = x f


swapTwo :: Int -> Int -> [a] -> [a]
swapTwo i j xs = zipWith (\x y -> 
    if x == i then xs !! j
    else if x == j then xs !! i
    else y) [0..] xs


instance Monoid ReviewSchedule where
  mempty = ReviewSchedule [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  mappend (ReviewSchedule a) (ReviewSchedule b) = ReviewSchedule $ zipWith (+) a b


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
