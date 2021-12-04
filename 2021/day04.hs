{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (void)
import Data.List (find, findIndex, foldl', transpose)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Prelude hiding (pi)

data Board = Board
  { _bContent :: ![[Int]],
    _bUnmarked :: !(Set Int)
  }
  deriving (Show, Eq)

makeLenses ''Board

markNumber :: Int -> Board -> Maybe Board
markNumber n b =
  if b ^. bUnmarked . to (Set.member n)
    then Just $ b & bUnmarked %~ Set.delete n
    else Nothing

isWinning :: Board -> Bool
isWinning b = any check (b ^. bContent) || any check (b ^. bContent . to transpose)
  where
    check :: [Int] -> Bool
    check ns = Set.null $ Set.fromList ns `Set.intersection` (b ^. bUnmarked)

score :: Board -> Int
score b = foldl' (+) 0 $ b ^. bUnmarked

data ProblemInput = ProblemInput
  { _piNumbers :: ![Int],
    _piBoards :: ![Board]
  }
  deriving (Show, Eq)

makeLenses ''ProblemInput

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

play :: ProblemInput -> [[Board]]
play pi = scanl step (pi ^. piBoards) (pi ^. piNumbers)
  where
    step acc n = map (\b -> fromMaybe b (markNumber n b)) acc

countWinners :: [Board] -> Int
countWinners = length . filter isWinning

solvePart1 :: ProblemInput -> Maybe Int
solvePart1 pi = do
  (n, bs) <- find (\(_, bs) -> countWinners bs == 1) $ (pi ^. piNumbers) `zip` tail (play pi)
  b <- find isWinning bs
  pure $ n * score b

solvePart2 :: ProblemInput -> Maybe Int
solvePart2 pi = do
  let rounds = (pi ^. piNumbers) `zip` tail (play pi)
      maxWinners = maximum $ map (countWinners . snd) rounds
  i <- findIndex (\(_, bs) -> countWinners bs == maxWinners) rounds
  let winState = rounds !! i
      winState' = rounds !! (i -1)
      winNumber = fst winState
  x <- find (\(b, b') -> isWinning b /= isWinning b') $ snd winState' `zip` snd winState
  pure (winNumber * score (snd x))

parseInput :: Text -> ProblemInput
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left e -> error (show e)
    Right r -> r

parser :: Parsec Text () ProblemInput
parser = ProblemInput <$> randomNumberParser <*> Parsec.many1 (Parsec.newline *> boardParser)

randomNumberParser :: Parsec Text () [Int]
randomNumberParser = numberParser `Parsec.sepBy` Parsec.char ',' <* Parsec.newline

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit

boardParser :: Parsec Text () Board
boardParser = do
  c <- contentP
  pure $ Board c (foldMap Set.fromList c)
  where
    blanks = Parsec.many (Parsec.char ' ')
    contentP =
      Parsec.many1
        ( blanks
            *> numberParser `Parsec.sepBy1` blanks
            <* (void Parsec.newline <|> void Parsec.eof)
        )
