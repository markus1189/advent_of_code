{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Parallel.Strategies (parMap, rseq)
import Data.List (foldl', sort)
import qualified Data.Text.IO as TIO
import Parsers (commaSeperatedNumbersParser, parseInput)

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput commaSeperatedNumbersParser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

genericSolve :: ([Int] -> Int) -> (Int -> Int -> Int) -> [Int] -> Int
genericSolve summarize calcFuel ns =
  minimum
    . parMap rseq (calculate calcFuel ns)
    $ [summarize ns .. maximum ns]

solvePart1 :: [Int] -> Int
solvePart1 = genericSolve median fuel1

solvePart2 :: [Int] -> Int
solvePart2 = genericSolve mean fuel2

calculate :: (Int -> Int -> Int) -> [Int] -> Int -> Int
calculate f ns p = foldl' (+) 0 $ map (f p) ns

fuel1 :: Num c => c -> c -> c
fuel1 p = abs . subtract p

fuel2 :: Integral a => a -> a -> a
fuel2 from to = n * (n + 1) `div` 2
  where
    n = abs $ to - from

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

mean :: Foldable t => t Int -> Int
mean xs = foldl' (+) 0 xs `div` length xs
