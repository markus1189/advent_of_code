-- New solution stolen shamelessly from https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Applicative ((<|>))
import           Control.Monad ((>=>), void)
import           Control.Parallel.Strategies (using, rpar, rseq, evalTuple2)
import           Data.Functor.Foldable (hylo)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Generics (Generic)
import           Text.Parsec (Parsec, (<?>))
import qualified Text.Parsec as Parsec

data Rule a = Simple Char
            | Compound [[a]]
            deriving stock (Show, Eq, Ord, Functor, Generic)

main :: IO ()
main = do
  input <- TIO.getContents
  let s1 = solvePart1 . parseInput $ input
      s2 = solvePart2 . parseInput $ input
  print ((s1,s2) `using` evalTuple2 rpar rseq) -- (147, 263)

solvePart1 :: (Map Int (Rule Int), [String]) -> Int
solvePart1 (rules, ss) = genericSolve rules ss

solvePart2 :: (Map Int (Rule Int), [String]) -> Int
solvePart2 (rules, ss) = genericSolve (extraRules <> rules) ss

extraRules :: Map Int (Rule Int)
extraRules = Map.fromList [ (8, Compound [[42],[42,8]])
                          , (11, Compound [[42,31], [42,11,31]])]

genericSolve :: Map Int (Rule Int) -> [String] -> Int
genericSolve mp = length . filter (any null . matcher mp)

generateAlg :: Map Int (Rule Int) -> Int -> Rule Int
generateAlg mp i = mp Map.! i

matchAlg :: Rule (String -> [String]) -> String -> [String]
matchAlg = \case
  Simple c -> \case
    [] -> []
    (x:xs) | c == x -> [xs]
           | otherwise -> []
  Compound css -> \str -> concatMap (sequenceAll str) css
 where
    sequenceAll :: String -> [String -> [String]] -> [String]
    sequenceAll s0 fs = foldr (>=>) pure fs s0

matcher :: Map Int (Rule Int) -> String -> [String]
matcher mp = hylo matchAlg (generateAlg mp) 0

parseInput :: Text -> (Map Int (Rule Int), [String])
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () (Map Int (Rule Int), [String])
parser = (,) <$> (Map.fromList <$> Parsec.many1 (ruleParser <* Parsec.newline))
             <* Parsec.newline
             <*> Parsec.many1 (Parsec.many1 (Parsec.oneOf "ab") <* Parsec.newline) <* Parsec.eof

numParser :: (Read n, Num n) => Parsec Text () n
numParser = read <$> Parsec.many1 Parsec.digit

ruleParser :: Parsec Text () (Int, Rule Int)
ruleParser = do
  ruleId <- numParser
  void $ Parsec.string ": "
  (ruleId,) <$> ruleBodyParser

ruleBodyParser :: Parsec Text () (Rule Int)
ruleBodyParser = ruleCharParser <|> concatParser
  where
    concatParser = do
      ruleIds <- ruleConcatParser
      optionalOr <- Parsec.optionMaybe $ Parsec.string "| " *> ruleConcatParser
      pure $ case optionalOr of
        Nothing -> Compound [ruleIds]
        Just other -> Compound [ruleIds, other]

ruleCharParser :: Parsec Text () (Rule a)
ruleCharParser = Simple <$> (Parsec.char '"' *> Parsec.anyChar <* Parsec.char '"') <?> "simple rule"

ruleConcatParser :: Parsec Text () [Int]
ruleConcatParser = Parsec.sepEndBy1 numParser (Parsec.char ' ') <?> "ruleConcat"
