{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Algorithm.Search (aStar)
import Control.Applicative ((<|>))
import Control.Lens.Combinators
    ( withIndex, FoldableWithIndex(ifolded) )
import Control.Lens.Operators ( (&), (^..), (^.), (<.>), (%~) )
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Linear (V2 (V2), _x, _y)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [[Int]] -> Maybe Int
solvePart1 is = fst <$> aStar nbs cost (manhattan target) (== target) (V2 0 0)
  where
    cost _ nbTo = fromMaybe (maxValue + 1) $ Map.lookup nbTo m
    m = toMap is
    maxValue = 999999
    target = fst $ Map.findMax m

solvePart2 :: [[Int]] -> Maybe Int
solvePart2 is = fst <$> aStar nbs cost (manhattan target) (== target) (V2 0 0)
  where
    cost _ nbTo = fromMaybe (maxValue + 1) $ modifiedLookup (width, height) nbTo m
    m = toMap is
    maxValue = 999999
    target = V2 (5 * width -1) (5 * height -1)
    width = length (head is)
    height = length is

manhattan :: Num a => V2 a -> V2 a -> a
manhattan v1 v2 = abs (v1 ^. _x - v2 ^. _x) + abs (v1 ^. _y - v2 ^. _y)

modifiedLookup :: (Int, Int) -> V2 Int -> Map (V2 Int) Int -> Maybe Int
modifiedLookup (dx, dy) v m =
  if v ^. _x < 0 || v ^. _y < 0 || v ^. _x `div` dx >= 5 || v ^. _y `div` dy >= 5
    then Nothing
    else (\x -> if x > 9 then x `mod` 9 else x) . (+ toAdd) <$> Map.lookup v' m
  where
    toAdd = v & _x %~ (`div` dx) & _y %~ (`div` dy) & \(V2 px py) -> px + py
    v' = v & _x %~ (`mod` dx) & _y %~ (`mod` dy)

nbs :: V2 Int -> [V2 Int]
nbs c = map (c +) . filter ((/= 1) . abs) . filter (/= 0) . sequence $ pure [1, 0, -1]

toMap :: FoldableWithIndex Int f => f (f a) -> Map (V2 Int) a
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
