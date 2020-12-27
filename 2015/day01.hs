{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Applicative ((<|>))
import           Data.List (foldl', scanl', find)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Direction = Up | Down deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print  . parseInput $ input
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: [Direction] -> Int
solvePart1 = foldl' step 0
  where step acc Up = acc + 1
        step acc Down = acc - 1

solvePart2 :: [Direction] -> Maybe Int
solvePart2 = fmap fst . find (\case (_,x) -> x == negate 1) . zip @Int [0..] . scanl' @Int step 0
  where step acc Up = acc + 1
        step acc Down = acc - 1

parseInput :: Text -> [Direction]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Direction]
parser = Parsec.many1 (Up <$ Parsec.char '(' <|> Down <$ Parsec.char ')') <* Parsec.newline <* Parsec.eof
