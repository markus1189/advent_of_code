{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
  ( Field2 (_2),
    filtered,
    preuse,
    view,
    _head,
  )
import Control.Lens.Operators ((%=))
import Control.Monad (void)
import Control.Monad.State (runState)
import Control.Monad.State.Class (MonadState)
import Data.List (find, foldl', sort)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Token = Parens | Braces | Brackes | Angles deriving (Show, Eq, Ord)

data Type = Open Token | Close Token deriving (Show, Eq, Ord)

data Result = Valid | Incomplete | Corrupted Char Char deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [[Char]] -> Int
solvePart1 cs = foldl' (+) 0 . map score . filter p $ map (classify . flip runState [] . mapM consume) cs
  where
    p (Corrupted _ _) = True
    p _ = False

solvePart2 :: [[Char]] -> Int
solvePart2 =
  median
    . sort
    . map
      ( scoreCompletion
          . autocomplete
          . view (filtered p . _2 . _2)
          . ( (\x -> (classify x, x))
                . flip runState []
                . mapM consume
            )
      )
  where
    p (Incomplete, _) = True
    p _ = False

median :: [a] -> a
median xs = xs !! (length xs `div` 2)

scoreCompletion :: String -> Int
scoreCompletion =
  foldl'
    ( \acc c ->
        5 * acc + case c of
          ')' -> 1
          ']' -> 2
          '}' -> 3
          '>' -> 4
          _ -> 0
    )
    0

autocomplete :: String -> String
autocomplete = map closing

classify :: ([Result], [Char]) -> Result
classify (cs, s) =
  case find
    ( \case
        (Corrupted _ _) -> True
        _ -> False
    )
    cs of
    Nothing -> if null s && last cs == Valid then Valid else Incomplete
    Just x -> x

score :: Result -> Int
score (Corrupted _ ')') = 3
score (Corrupted _ ']') = 57
score (Corrupted _ '}') = 1197
score (Corrupted _ '>') = 25137
score _ = 0

consume :: MonadState [Char] m => Char -> m Result
consume c = do
  if isOpen c
    then push c
    else do
      c' <- pop
      case c' of
        Nothing -> pure Incomplete
        Just c'' -> if matches c'' c then pure Valid else pure (Corrupted (closing c'') c)

push :: MonadState [Char] m => Char -> m Result
push c = id %= (c :) >> pure Incomplete

pop :: MonadState [Char] m => m (Maybe Char)
pop = do
  mc <- preuse _head
  id %= drop 1
  pure mc

isOpen :: Char -> Bool
isOpen = (`elem` ['(', '{', '[', '<'])

matches :: Char -> Char -> Bool
matches '(' ')' = True
matches '{' '}' = True
matches '[' ']' = True
matches '<' '>' = True
matches _ _ = False

closing :: Char -> Char
closing '(' = ')'
closing '{' = '}'
closing '[' = ']'
closing '<' = '>'
closing _ = undefined

parser :: Parsec Text () [[Char]]
parser = Parsec.many1 (Parsec.many1 (Parsec.oneOf "(){}[]<>") <* (void Parsec.newline <|> void Parsec.eof))

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r
