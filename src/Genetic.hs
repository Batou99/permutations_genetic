module Genetic where

fitness :: List Int -> (List Int -> Number) -> Number
fitness xs = foldl (x-avg(xs))^2 0 xs
  where
    avg xs = sum xs / fromIntegral (length xs)

genetic_selection :: List (List Int) -> (List Int -> List Int) -> List (List Int)
genetic_selection population evolve_function =
  undefined

