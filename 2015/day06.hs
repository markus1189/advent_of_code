{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Debug.Trace
import Data.Foldable (foldl')
import           Control.Applicative ((<|>))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


newtype Rect = Rect ((Int, Int),(Int, Int)) deriving newtype (Show, Eq, Ord)

data Operation = TurnOn | TurnOff | Toggle deriving stock (Show, Eq, Ord)

data Command = Command Operation Rect deriving (Show, Eq, Ord)

gridHeight :: Int
gridHeight = 1000

main :: IO ()
main = do
  input <- TIO.getContents
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: _ -> _
solvePart1 = length . IntMap.filter id . foldl' eval (IntMap.fromDistinctAscList (map (,False) $ coords (Rect ((0,0),(999,999)))))

solvePart2 :: _ -> _
solvePart2 = IntMap.foldl (+) 0 . foldl' eval2 (IntMap.fromDistinctAscList (map (,0) $ coords (Rect ((0,0),(999,999)))))

eval :: IntMap Bool -> Command -> IntMap Bool
eval m (Command Toggle r) = fmap not (IntMap.restrictKeys m (IntSet.fromDistinctAscList (coords r))) `IntMap.union` m
eval m (Command TurnOn r) = IntMap.fromDistinctAscList (map (,True) (coords r)) `IntMap.union` m
eval m (Command TurnOff r)  = IntMap.fromDistinctAscList (map (,False) (coords r)) `IntMap.union` m

eval2 :: IntMap Int -> Command -> IntMap Int
eval2 m (Command Toggle r) = IntMap.union (fmap (+2) (IntMap.restrictKeys m (IntSet.fromDistinctAscList (coords r)))) m
eval2 m (Command TurnOn r) = IntMap.union (fmap (+1) (IntMap.restrictKeys m (IntSet.fromDistinctAscList (coords r)))) m
eval2 m (Command TurnOff r)  = IntMap.union (fmap (max 0 . subtract 1) (IntMap.restrictKeys m (IntSet.fromDistinctAscList (coords r)))) m

parseInput :: Text -> _
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () _
parser = Parsec.many1 lineParser <* Parsec.eof

lineParser = Command <$> operationParser <* Parsec.char ' ' <*> rectParser <* Parsec.newline

pairParser = (,) <$> numberParser <* Parsec.char ',' <*> numberParser

rectParser :: Parsec Text () Rect
rectParser = fmap Rect $ (,) <$> pairParser <* Parsec.string " through " <*> pairParser

operationParser :: Parsec Text () Operation
operationParser = Parsec.char 't' *> Parsec.choice [ TurnOn <$ Parsec.try (Parsec.string "urn on")
                                                   , TurnOff <$ Parsec.string "urn off"
                                                   , Toggle <$ Parsec.string "oggle"
                                                   ]

numberParser :: (Read a, Num a) => Parsec Text () a
numberParser = read <$> Parsec.many1 Parsec.digit

coords :: Rect -> [Int]
coords (Rect ((x1,y1), (x2,y2))) = do
  y <- [y1..y2]
  x <- [x1..x2]
  pure $ x + y * gridHeight
{-# INLINE coords #-}
