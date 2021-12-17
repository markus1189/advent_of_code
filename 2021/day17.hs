{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens.Combinators
  ( Field1 (_1),
    Field2 (_2),
    makeLenses,
    view,
  )
import Control.Lens.Operators ((^.))
import Data.Foldable (maximumBy)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Linear.V2 (V2 (V2), _x, _y)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Target = Target
  { _tgtX :: (Int, Int),
    _tgtY :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

makeLenses ''Target

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print parsed
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: Target -> Int
solvePart1 area = maxHeight . snd . maximumBy (comparing $ maxHeight . snd) . filter (hitsTarget area . snd) $ trajectories
  where
    vs = do
      let dx = optimalVx
      V2 dx <$> [dx .. 100]
    trajectories = map (\v -> (v, filteredTrajectory area v)) vs
    optimalVx = fromJust $ find (\n -> sumUp n >= area ^. tgtX . _1) [1 .. (area ^. tgtX . _1)]

filteredTrajectory :: Target -> V2 Int -> [V2 Int]
filteredTrajectory area =
  takeWhile
    ( \p ->
        let a = (p ^. _x <= area ^. tgtX . _2)
            b = (p ^. _y >= area ^. tgtY . _1)
         in a && b
    )
    . trajectory

hitsTarget :: Target -> [V2 Int] -> Bool
hitsTarget (Target (minX, maxX) (minY, maxY)) = any (\(V2 px py) -> px >= minX && px <= maxX && py >= minY && py <= maxY)

maxHeight :: [V2 Int] -> Int
maxHeight vs = if null vs then 0 else view _y $ maximumBy (comparing $ view _y) vs

trajectory :: V2 Int -> [V2 Int]
trajectory = go (V2 0 0)
  where
    go :: V2 Int -> V2 Int -> [V2 Int]
    go p v = let p' = p + v in p : go p' (stepV v)
    stepV :: V2 Int -> V2 Int
    stepV v = V2 x' y'
      where
        x' = let x = v ^. _x in x - signum x
        y' = v ^. _y - 1

sumUp :: Integral a => a -> a
sumUp n = n * (n + 1) `div` 2

solvePart2 :: Target -> Int
solvePart2 area = length . filter (hitsTarget area . snd) $ trajectories
  where
    vs = do
      dx <- [1 .. (area ^. tgtX . _2)]
      V2 dx <$> [(area ^. tgtY . _1) .. (negate $ area ^. tgtY . _1)]
    trajectories = map (\v -> (v, filteredTrajectory area v)) vs

parser :: Parsec Text () Target
parser =
  Target <$> (Parsec.string "target area: x=" *> intervalP) <*> (Parsec.string ", y=" *> intervalP) <* Parsec.eof
  where
    intervalP = (,) <$> numberParser <*> (Parsec.string ".." *> numberParser)

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

numberParser :: Parsec Text () Int
numberParser = do
  sign <- Parsec.option 1 (Parsec.char '-' $> (-1))
  (* sign) . read @Int <$> Parsec.many1 Parsec.digit
