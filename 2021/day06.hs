{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
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

solvePart1 :: [IntMap.Key] -> Integer
solvePart1 = genericSolve 80

solvePart2 :: [IntMap.Key] -> Integer
solvePart2 = genericSolve 256

genericSolve :: Int -> [IntMap.Key] -> Integer
genericSolve n =
  IntMap.foldl' (+) 0
    . (!! n)
    . iterate step
    . IntMap.fromListWith (+)
    . map (,1 :: Integer)

step :: IntMap Integer -> IntMap Integer
step m =
  IntMap.updateLookupWithKey (\_ _ -> Nothing) 0 m
    & _1 %~ maybe IntMap.empty (IntMap.fromList . (\x -> [(8, x), (6, x)]))
    & _2 %~ IntMap.mapKeysMonotonic (subtract 1)
    & uncurry (IntMap.unionWith (+))

parseInput :: Text -> [Int]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Int]
parser = numberParser `Parsec.sepBy1` Parsec.char ',' <* Parsec.many Parsec.newline <* Parsec.eof

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit
