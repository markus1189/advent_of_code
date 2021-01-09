{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Maybe (isJust)
import qualified Control.Monad.State as State
import Data.Foldable.Extra (findM)
import Control.Applicative (ZipList(..))
import           Control.DeepSeq (NFData)
import           Control.Foldl (Fold)
import qualified Control.Foldl as Fold
import           Control.Parallel.Strategies (rseq, withStrategy, parList)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

data Step = Step { stepPrev :: !(Maybe (Char, Char))
                 , stepPairs :: !(Set (Char, Char))
                 , stepSuccess :: !Bool
                 }
          deriving (Show, Eq, Ord, Generic, NFData)

main :: IO ()
main = do
  input <- lines <$> getContents
  print . solvePart1 $ input
  print . solvePart2 $ input

isNice :: Fold Char Bool
isNice = and <$> sequenceA [threeVowels, noneFromBlacklist, atLeastOneRepeatedtwice]

threeVowels :: Fold Char Bool
threeVowels = (>= 3) <$> Fold.prefilter (`Set.member` vowelSet) Fold.length

vowelSet :: Set Char
vowelSet = Set.fromList "aeiou"

noneFromBlacklist :: Fold Char Bool
noneFromBlacklist = Fold.Fold go (Nothing, True) snd
  where go (_, False) _ = (Nothing, False)
        go (Nothing, _) c = (Just c, True)
        go (Just prevC, acc) c = (Just c, acc && not ([prevC,c] `Set.member` blacklist))

atLeastOneRepeatedtwice :: Fold Char Bool
atLeastOneRepeatedtwice = Fold.Fold go (Nothing, False) snd
  where go (_, True) _ = (Nothing, True)
        go (Nothing, _) c = (Just c, False)
        go (Just prevC, acc) c = (Just c, acc || prevC == c)

blacklist :: Set String
blacklist = Set.fromList @String ["ab", "cd", "pq", "xy"]

findSeenBefore :: (Ord a, Foldable f) => f a -> Bool
findSeenBefore xs = isJust . flip State.evalState Set.empty $ findM seenBefore xs
  where seenBefore x = do
          seen <- State.get
          if x `Set.member` seen
            then pure True
            else do
              State.modify (Set.insert x)
              pure False

letterPairOccursTwice :: (Ord a, Eq a) => [a] -> Bool
letterPairOccursTwice = findSeenBefore . removeAdjacent . pairs

letterInTheMiddle :: Fold (Char, Char, Char) Bool
letterInTheMiddle = (>=1) <$> Fold.prefilter (\(x,_,z) -> x == z) Fold.length

isNice2 :: String -> Bool
isNice2 xs = letterPairOccursTwice xs && Fold.fold letterInTheMiddle (triples xs)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

triples :: [a] -> [(a,a,a)]
triples xs = getZipList $ (,,) <$> ZipList xs <*> ZipList (tail xs) <*> ZipList (tail (tail xs))

removeAdjacent :: Eq a => [a] -> [a]
removeAdjacent = snd . foldr go (Nothing, [])
  where go x (Nothing, acc) = (Just x, x : acc)
        go y (Just x, acc) = if x == y then (Nothing, acc) else (Just y, y : acc)

solvePart1 :: [String] -> Int
solvePart1 = length . filter id . withStrategy (parList rseq) . map (Fold.fold isNice)

solvePart2 :: [String] -> Int
solvePart2 = length . filter id . withStrategy (parList rseq) . map isNice2
