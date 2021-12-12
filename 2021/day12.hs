{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
import Control.Monad (void)
import Data.Char (isLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Tree (Tree (Node), unfoldTree)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [(String, String)] -> Int
solvePart1 es = lengthOf (folded . filtered (== "end")) $ prune1 (Set.singleton "start") $ unfoldTree (\cur -> (cur, fromMaybe [] (Map.lookup cur m))) "start"
  where
    m = buildMap es

prune1 :: Set String -> Tree String -> Tree String
prune1 seen (Node x xs) = Node x xs'
  where
    xs' = map (\n@(Node x' _) -> prune1 (if all isLower x' then Set.insert x' seen else seen) n) unseenNext
    unseenNext = filter (\(Node x' _) -> not (Set.member x' seen)) xs

solvePart2 :: [(String, String)] -> Int
solvePart2 es =
  length $ filter ((== "end") . last) $ toPaths $ prune2 (Map.singleton "start" 0) $ unfoldTree (\cur -> (cur, fromMaybe [] (Map.lookup cur m))) "start"
  where
    m = buildMap es

prune2 :: Map String Int -> Tree String -> Tree String
prune2 seen (Node x xs) = if x == "end" then Node x [] else Node x xs'
  where
    xs' = map (\n@(Node x' _) -> prune2 (if all isLower x' then Map.insertWith (+) x' 1 seen else seen) n) unseenNext
    unseenNext = filter (\(Node x' _) -> not (Map.member x' seen) || all (<= 1) seen) xs

toPaths :: Tree String -> [[String]]
toPaths (Node x []) = [[x]]
toPaths (Node x xs) = map (x :) (concatMap toPaths xs)

buildMap :: [(String, String)] -> Map String [String]
buildMap es = Map.fromListWith (<>) $ filter (\(f, ts) -> not ("start" `elem` ts || f == "end")) $ concatMap (\(f, t) -> [(f, [t]), (t, [f])]) es

parser :: Parsec Text () [(String, String)]
parser = Parsec.many1 (edgeParser <* (void Parsec.newline <|> void Parsec.eof))

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

edgeParser :: Parsec Text () (String, String)
edgeParser = (,) <$> Parsec.many1 Parsec.letter <* Parsec.char '-' <*> Parsec.many1 Parsec.letter
