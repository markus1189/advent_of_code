-- Improved version STOLEN FROM https://github.com/mstksg/advent-of-code-2020/blob/d042ebb16a6fbb4169f357b94a2018dab4e43851/src/AOC/Challenge/Day17.hs
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Parallel.Strategies (parMap, rseq, rpar, runEval)
import           Control.Lens.Combinators (lined, folded, asIndex, to, filtered)
import           Control.Lens.Operators ((<.>), (&), (.~))
import           Control.Monad (guard)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.Lens (setOf)
import           Linear (R2(..), V2(..), V3(..), V4(..))

main :: IO ()
main = do
  input <- getContents
  print $ solvePart1 . tiles $ input -- 368
  print $ solvePart2 . tiles $ input -- 2696

solvePart1 :: Set (V3 Int) -> Int
solvePart1 = genericSolve

solvePart2 :: Set (V4 Int) -> Int
solvePart2 = genericSolve

genericSolve :: (Traversable f, Applicative f, Num (f Int), Ord (f Int)) => Set (f Int) -> Int
genericSolve = Set.size . (!!10) . iterate step

neighbors :: (Traversable f, Applicative f, Ord (f a), Num a, Num (f a)) => f a -> Set (f a)
neighbors p = Set.fromList $ do
  delta <- sequence (pure [-1, 0, 1])
  guard $ delta /= pure 0
  pure $ p + delta

spreadToMap :: (Traversable f, Applicative f, Ord (f a), Num a, Num (f a)) => Set (f a) -> Map (f a) Int
spreadToMap = Map.unionsWith (+) . parMap rseq (Map.fromSet (const 1) . neighbors) . Set.toList

step :: (Traversable f, Applicative f, Ord (f a), Num a, Num (f a)) => Set (f a) -> Set (f a)
step ps = runEval $ do
  stay <- rpar stayAlive
  come <- rpar comeAlive
  _ <- rseq stay
  _ <- rseq come
  pure (stay <> come)
  where
    neighborCounts = spreadToMap ps
    stayAlive = Map.keysSet . Map.filter ((||) <$> (==2) <*> (==3)) $ neighborCounts `Map.restrictKeys` ps
    comeAlive = Map.keysSet . Map.filter (== 3) $ neighborCounts `Map.withoutKeys`  ps

tiles :: (Ord (f Int), R2 f, Applicative f) => String -> Set (f Int)
tiles = setOf $ (lined <.> folded)
              . filtered (=='#')
              . asIndex
              . to (\(y,x) -> pure 0 & _xy .~ V2 x y)
