{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Data.Functor.Identity (Identity)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Inst = Forward Int | Down Int | Up Int deriving (Show, Eq, Ord)

data Position = Position
  { _posHorizontal :: Int,
    _posDepth :: Int,
    _posAim :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Position

initialPosition :: Position
initialPosition = Position 0 0 0

calcSolution :: Position -> Int
calcSolution (Position h d _) = d * h

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput input
  print parsed
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [Inst] -> Int
solvePart1 = calcSolution . foldl' step initialPosition
  where
    step :: Position -> Inst -> Position
    step p (Forward x) = p & posHorizontal +~ x
    step p (Down x) = p & posDepth +~ x
    step p (Up x) = p & posDepth -~ x

solvePart2 :: [Inst] -> Int
solvePart2 = calcSolution . foldl' step initialPosition
  where
    step :: Position -> Inst -> Position
    step p (Forward x) = p & posHorizontal +~ x & posDepth +~ (p ^. posAim * x)
    step p (Down x) = p & posAim +~ x
    step p (Up x) = p & posAim -~ x

parseInput :: Text -> [Inst]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left e -> error (show e)
    Right r -> r

instParser :: Parsec Text () Inst
instParser =
  Parsec.choice
    [ Parsec.string "forward" *> Parsec.space *> (Forward <$> (read @Int <$> Parsec.many1 Parsec.digit)),
      Parsec.string "down" *> Parsec.space *> (Down <$> (read @Int <$> Parsec.many1 Parsec.digit)),
      Parsec.string "up" *> Parsec.space *> (Up <$> (read @Int <$> Parsec.many1 Parsec.digit))
    ]

parser :: Parsec.ParsecT Text () Identity [Inst]
parser = Parsec.many1 (instParser <* Parsec.newline) <* Parsec.eof
