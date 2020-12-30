{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Lens.Combinators (over, view, both)
import           Data.List (scanl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Sum(..))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Direction = DUp | DRight | DDown | DLeft deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: [Direction] -> Int
solvePart1 = Map.size . countBuildings

solvePart2 :: [Direction] -> Int
solvePart2 ds = Map.size $ view both $ over both countBuildings (santaDirs, roboDirs)
  where (santaDirs, roboDirs) = uninterleave ds

countBuildings :: [Direction] -> Map (Sum Int, Sum Int) (Sum Int)
countBuildings = Map.fromListWith (<>) . map (,Sum @Int 1) . scanl' (\acc d -> acc <> directionToOffset d) (0,0)

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (xs, ys) -> (x:ys, xs)) ([],[])

directionToOffset :: Direction -> (Sum Int, Sum Int)
directionToOffset DUp = (0, 1)
directionToOffset DRight = (1, 0)
directionToOffset DDown = (0, -1)
directionToOffset DLeft = (-1, 0)

parseInput :: Text -> [Direction]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Direction]
parser = Parsec.many1 directionParser <* Parsec.newline <* Parsec.eof

directionParser :: Parsec Text () Direction
directionParser = Parsec.choice [ DUp <$ Parsec.char '^'
                                , DRight <$ Parsec.char '>'
                                , DDown <$ Parsec.char 'v'
                                , DLeft <$ Parsec.char '<'
                                ]
