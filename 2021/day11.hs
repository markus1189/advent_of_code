{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
  ( FoldableWithIndex (ifolded),
    Ixed (ix),
    withIndex,
    _Just,
  )
import Control.Lens.Operators ((&), (+~), (.~), (<.>), (^..))
import Control.Lens.Type (Traversal)
import Control.Monad (replicateM, void)
import Control.Monad.Loops (untilM)
import Control.Monad.State (MonadState (state), execState, gets, runState)
import qualified Control.Monad.State as State
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Linear.V2 (V2 (V2))
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

type Grid a = Map (V2 Int) a

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [[Int]] -> Int
solvePart1 xxs = foldl' (+) 0 . flip execState (toMap xxs) $ replicateM 100 step

solvePart2 :: [[Int]] -> Int
solvePart2 xxs =
  length . flip execState (toMap xxs) $
    step `untilM` gets ((== 100) . length . Map.filter (== 0))

step :: MonadState (Grid Int) m => m Int
step = State.state (clear . fixpoint activateAll . Map.map (Just . (+ 1)))

activateAll :: Grid (Maybe Int) -> Grid (Maybe Int)
activateAll g = foldl' activate g . Map.keys . Map.filter (any (> 9)) $ g

activate :: Grid (Maybe Int) -> V2 Int -> Grid (Maybe Int)
activate m pos = m & ix pos .~ Nothing & nbsL pos . _Just +~ 1

fixpoint :: Eq b => (b -> b) -> b -> b
fixpoint f x = fst . fromJust . find (uncurry (==)) $ fs `zip` tail fs
  where
    fs = iterate f x

clear :: Grid (Maybe Int) -> (Int, Grid Int)
clear m = (length (Map.filter null m), Map.map (fromMaybe 0) m)

nbsL :: V2 Int -> Traversal (Grid a) (Grid a) a a
nbsL c focus = Map.traverseWithKey (\k v -> if k `Set.member` nbs then focus v else pure v)
  where
    nbs = Set.fromList $ map (c +) . filter (/= 0) . sequence $ pure [1, 0, -1]

toMap :: FoldableWithIndex Int f => f (f a) -> Grid a
toMap xxs = xxs ^.. (ifolded <.> ifolded) . withIndex & Map.fromList & Map.mapKeys (uncurry V2)

parser :: Parsec Text () [[Int]]
parser =
  Parsec.many1 $
    Parsec.many1 (read @Int . pure <$> Parsec.digit)
      <* (void Parsec.newline <|> void Parsec.eof)

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r
