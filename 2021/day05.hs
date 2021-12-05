{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Linear.V2 as V2
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Line = Line
  { _lineStart :: V2 Int,
    _lineEnd :: V2 Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Line

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [Line] -> Int
solvePart1 = solvePart2 . filter ((||) <$> isHorizontal <*> isVertical)

solvePart2 :: [Line] -> Int
solvePart2 =
  Map.size
    . Map.filter (> 1)
    . Map.fromListWith (+)
    . fmap (,1 :: Int)
    . concatMap explode

isHorizontal :: Line -> Bool
isHorizontal l = l ^. lineStart . _x == l ^. lineEnd . _x

isVertical :: Line -> Bool
isVertical l = l ^. lineStart . _y == l ^. lineEnd . _y

explode :: Line -> [V2 Int]
explode l = takeWhile (<= end) $ iterate (+ n) start
  where
    [start, end] = sort [l ^. lineStart, l ^. lineEnd]
    n = signum <$> (end - start)

parseInput :: Text -> [Line]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Line]
parser = Parsec.many1 $ rowParser <* (void Parsec.newline <|> void Parsec.eof)

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit

rowParser :: Parsec Text () Line
rowParser = Line <$> pointParser <* Parsec.string " -> " <*> pointParser
  where
    pointParser = V2 <$> numberParser <* Parsec.char ',' <*> numberParser
