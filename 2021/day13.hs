{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (void)
import Data.List (foldl', intercalate)
import Data.List.Split (chunksOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Linear.V2
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data PaperFold = FoldX Int | FoldY Int deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print parsed
  print $ solvePart1 parsed
  putStrLn $ solvePart2 parsed

solvePart1 :: (Set (V2 Int), [PaperFold]) -> Int
solvePart1 (ps, fs) = maybe 0 Set.size $ preview (_head . to (foldPaper ps)) fs

solvePart2 :: (Set (V2 Int), [PaperFold]) -> String
solvePart2 (ps, fs) = draw $ foldl' foldPaper ps fs

foldPaper :: Set (V2 Int) -> PaperFold -> Set (V2 Int)
foldPaper ps (FoldX dx) = genFoldPaper _x ps dx
foldPaper ps (FoldY dy) = genFoldPaper _y ps dy

genFoldPaper :: Lens' (V2 Int) Int -> Set (V2 Int) -> Int -> Set (V2 Int)
genFoldPaper l ps d = Set.union untouched (Set.map (\p -> p & l .~ d - (p ^. l - d)) toBeFolded)
  where
    (toBeFolded, untouched) = Set.partition (\p -> p ^. l > d) ps

draw :: Set (V2 Int) -> String
draw ps = intercalate "\n" $
  chunksOf (maxX + 1) $ do
    y <- [0 .. maxY]
    x <- [0 .. maxX]
    pure $ if Set.member (V2 x y) ps then '#' else ' '
  where
    maxX = maximum $ Set.map (view _x) ps
    maxY = maximum $ Set.map (view _y) ps

parser :: Parsec Text () (Set (V2 Int), [PaperFold])
parser =
  (,)
    <$> (Set.fromList <$> Parsec.many1 (pointParser <* void Parsec.newline))
    <*> (Parsec.newline *> Parsec.many1 (foldParser <* (void Parsec.newline <|> void Parsec.eof)))

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit

pointParser :: Parsec.ParsecT Text () Identity (V2 Int)
pointParser = V2 <$> numberParser <* Parsec.char ',' <*> numberParser

foldParser :: Parsec.ParsecT Text () Identity PaperFold
foldParser =
  Parsec.string "fold along "
    *> Parsec.choice
      [ Parsec.string "x=" *> (FoldX <$> numberParser),
        Parsec.string "y=" *> (FoldY <$> numberParser)
      ]
