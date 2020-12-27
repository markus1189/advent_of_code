{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.List (unfoldr)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

import qualified Data.HashTable.ST.Linear as H

data Value a = First !a
             | Multiple !a !a
             deriving (Show, Eq)

newtype Turn = Turn { getTurn :: Int} deriving newtype (Show, Eq, Enum, Num)

data Memory = Memory !Turn !Int !(IntMap (Value Turn))
  deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input -- 1015
  print $ solvePart2 . parseInput $ input -- 201

initialMemory :: [Int] -> Memory
initialMemory is =
  Memory (Turn $ fromIntegral (length is + 1))
         (last is)
         (IntMap.fromList (is `zip` map (First . Turn) [1..]))

solvePart1 :: [Int] -> Int
solvePart1 = last . take 2020 . play

solvePart2 :: [Int] -> Int
solvePart2 = last . take 30000000 . play

play :: [Int] -> [Int]
play is = is ++ unfoldr step (initialMemory is)

step :: Memory -> Maybe (Int, Memory)
step mem@(Memory _ prev m) = case IntMap.lookup prev m of
  Just (First _) -> Just (0, updateMemory 0 mem)
  Just (Multiple t1 t2) -> Just (getTurn (t2-t1), updateMemory (getTurn $ t2-t1) mem)
  Nothing -> error "oops"

updateMemory :: Int -> Memory -> Memory
updateMemory i (Memory turn _ m) = Memory (turn + 1) i (IntMap.alter alter i m)
  where
    alter Nothing = Just (First turn)
    alter (Just (First t)) = Just (Multiple t turn)
    alter (Just (Multiple _ t)) = Just (Multiple t turn)

parseInput :: Text -> [Int]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Int]
parser = number `Parsec.sepBy` Parsec.char ','
  <* Parsec.newline
  <* Parsec.eof
  where number = read @Int <$> Parsec.many1 Parsec.digit
