{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Data.List (foldl', group, sort, transpose, unfoldr)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput input
  print parsed
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [[Bool]] -> Int
solvePart1 bits = fromBits gamma * fromBits epsilon
  where
    gamma = mostCommon <$> transpose bits
    epsilon = map not gamma

solvePart2 :: [[Bool]] -> Int
solvePart2 bits = fromBits (findOxygen bits) * fromBits (findCO2 bits)

findOxygen :: [[Bool]] -> [Bool]
findOxygen = findBits id

findCO2 :: [[Bool]] -> [Bool]
findCO2 = findBits not

findBits :: (Bool -> Bool) -> [[Bool]] -> [Bool]
findBits f bits = head . last $ unfoldr step (0, bits)
  where
    step :: (Int, [[Bool]]) -> Maybe ([[Bool]], (Int, [[Bool]]))
    step (col, bs) = if length bs <= 1 then Nothing else Just (bs', (col + 1, bs'))
      where
        curCol = cols !! col
        mcb = mostCommon curCol
        cols = transpose bs
        bs' = filter (\row -> row !! col == f mcb) bs

fromBits :: [Bool] -> Int
fromBits bs = foldl' (\acc (i, b) -> if b then acc + (2 :: Int) ^ i else acc) (0 :: Int) $ [(0 :: Int) ..] `zip` reverse bs

parseInput :: Text -> [[Bool]]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left e -> error (show e)
    Right r -> r

parser :: Parsec Text () [[Bool]]
parser = Parsec.many1 (bitsParser <* Parsec.newline) <* Parsec.eof

bitsParser :: Parsec Text () [Bool]
bitsParser = Parsec.many1 $ True <$ Parsec.char '1' <|> False <$ Parsec.char '0'

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort
