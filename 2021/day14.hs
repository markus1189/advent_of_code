{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (replicateM, void)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Rule = Rule {_ruleMatch :: String, _ruleReplace :: Char} deriving (Show, Eq, Ord)

makeLenses ''Rule

data S = S {_sCounts :: Map Char Int, _sPairs :: Map String Int} deriving (Show, Eq, Ord)

makeLenses ''S

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

mkS :: String -> S
mkS tmpl =
  S
    (Map.fromListWith (+) $ fmap (,1) tmpl)
    (Map.fromListWith (+) $ collectPairs tmpl)

collectPairs :: Num b => [a] -> [([a], b)]
collectPairs s = fmap (\(c1, c2) -> ([c1, c2], 1)) $ s `zip` tail s

genericSolve :: Int -> (String, [Rule]) -> S
genericSolve n (t, rs) = (!! n) $ iterate (step rs) $ mkS t

solvePart1 :: (String, [Rule]) -> Int
solvePart1 = answer . genericSolve 10

solvePart2 :: (String, [Rule]) -> Int
solvePart2 = answer . genericSolve 40

answer :: S -> Int
answer = ((-) <$> maximum <*> minimum) . Map.elems . view sCounts

step :: [Rule] -> S -> S
step rs s = s'
  where
    s' = s & sPairs .~ p' & sCounts %~ Map.unionWith (+) cs'
    p' = newPairs rs' (s ^. sPairs)
    cs' = newCounts (Map.fromListWith (+) $ map (\r -> (r, matchCount r s)) rs')
    rs' = matchingRules s rs

newPairs :: [Rule] -> Map String Int -> Map String Int
newPairs rs ps = Map.fromListWith (+) $ concatMap (\(k, v) -> maybe [(k, v)] (map (,v) . pairs) (find (\r -> k == r ^. ruleMatch) rs)) $ Map.toList ps

matchCount :: Rule -> S -> Int
matchCount r s = fromMaybe 0 $ s ^. sPairs . at (r ^. ruleMatch)

newCounts :: Map Rule Int -> Map Char Int
newCounts rs = Map.fromListWith (+) $ map (\(r, c) -> (r ^. ruleReplace, c)) $ Map.toList rs

matchingRules :: S -> [Rule] -> [Rule]
matchingRules s = filter (\r -> maybe False (> 0) $ s ^. sPairs . at (r ^. ruleMatch))

pairs :: Rule -> [String]
pairs r = (\[c1, c2] -> [[c1, c], [c, c2]]) $ r ^. ruleMatch
  where
    c = r ^. ruleReplace

parser :: Parsec Text () ([Char], [Rule])
parser = (,) <$> polymerTemplateParser <* Parsec.newline <* Parsec.newline <*> Parsec.many1 (ruleParser <* (void Parsec.newline <|> Parsec.eof))

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

polymerTemplateParser :: Parsec.ParsecT Text u Identity [Char]
polymerTemplateParser = Parsec.many1 Parsec.upper

ruleParser :: Parsec.ParsecT Text u Identity Rule
ruleParser =
  Rule
    <$> (replicateM 2 Parsec.upper <* Parsec.string " -> ")
    <*> Parsec.upper
