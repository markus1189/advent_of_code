{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators (Field2 (_2), makePrisms, taking, _1)
import Control.Lens.Operators ((&), (+~))
import Control.Monad (guard, void)
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Numeric.Natural (Natural)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a) deriving (Eq, Ord, Functor)

data Part a = V Natural a | B deriving (Eq, Show)

makePrisms ''Part

instance Show a => Show (BinTree a) where
  show (Leaf i) = show i
  show (Branch lhs rhs) = "[" <> show lhs <> "," <> show rhs <> "]"

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [BinTree Int] -> Int
solvePart1 (map (toPostorder 0) -> (n : ns)) = magnitude $ fromPostorder [] $ foldl' (\acc po -> normalize (acc `addParts` po)) n ns
solvePart1 _ = error "Invalid input"

addParts :: [Part a] -> [Part a] -> [Part a]
addParts p1s p2s = incDepth (p1s ++ p2s) ++ [B]
  where
    incDepth = traverse . _V . _1 +~ 1

solvePart2 :: [BinTree Int] -> Int
solvePart2 ns = maximum $
  map solvePart1 $ do
    n1 <- ns
    n2 <- ns
    guard $ n1 /= n2
    [[n1, n2], [n2, n1]]

normalize :: [Part Int] -> [Part Int]
normalize = fixpoint ((\(applied, x) -> if applied then x else doSplit x) . doExplode)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = let xs = iterate f x in fst $ fromJust $ find (uncurry (==)) $ xs `zip` tail xs

magnitude :: BinTree Int -> Int
magnitude (Leaf x) = x
magnitude (Branch lhs rhs) = 3 * magnitude lhs + 2 * magnitude rhs

toPostorder :: Natural -> BinTree a -> [Part a]
toPostorder d (Leaf x) = [V d x]
toPostorder d (Branch lhs rhs) = toPostorder (d + 1) lhs ++ toPostorder (d + 1) rhs ++ [B]

fromPostorder :: Show a => [BinTree a] -> [Part a] -> BinTree a
fromPostorder [] [] = error "1"
fromPostorder [x] [] = x
fromPostorder s (V _ x : ps) = fromPostorder (Leaf x : s) ps
fromPostorder [] (B : _) = error "2"
fromPostorder [_] (B : _) = error "3"
fromPostorder (x : y : xs) (B : ps) = fromPostorder (Branch y x : xs) ps
fromPostorder s ps = error $ show (s, ps)

doExplode :: (Show a, Num a) => [Part a] -> (Bool, [Part a])
doExplode = go []
  where
    go ps [] = (False, reverse ps)
    go ps (V d1 x : V d2 y : B : ns) | d1 == d2 && d1 == 5 = (True, reverse (addNeighbor x ps) ++ [V (5 - 1) 0] ++ addNeighbor y ns)
    go ps (n : ns) = go (n : ps) ns

doSplit :: [Part Int] -> [Part Int]
doSplit = go []
  where
    go :: [Part Int] -> [Part Int] -> [Part Int]
    go ps [] = reverse ps
    go ps (n@(V d v) : ns)
      | v >= 10 = reverse (B : V (d + 1) (ceiling (fromIntegral @_ @Double v / 2.0)) : V (d + 1) (floor (fromIntegral @_ @Double v / 2.0)) : ps) ++ ns
      | otherwise = go (n : ps) ns
    go ps (n : ns) = go (n : ps) ns

addNeighbor :: Num a => a -> [Part a] -> [Part a]
addNeighbor x ns = ns & taking 1 (traverse . _V) . _2 +~ x

parser :: Parsec Text () [BinTree Int]
parser = Parsec.many1 (sfnParser <* (void Parsec.newline <|> void Parsec.eof))

sfnParser :: Parsec Text () (BinTree Int)
sfnParser = Parsec.between (Parsec.char '[') (Parsec.char ']') (Branch <$> sfnParser <* Parsec.char ',' <*> sfnParser) <|> Leaf <$> numberParser

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit
