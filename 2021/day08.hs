{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Concurrent.Async (forConcurrently)
import Control.Lens.Combinators
  ( filtered,
    folded,
    lengthOf,
    makeLenses,
    view,
  )
import Control.Lens.Operators ((&), (^.), (^?!))
import Control.Monad (replicateM, void)
import Control.Monad.Holmes (Holmes, satisfying)
import Data.Hashable (Hashable)
import Data.Holmes (Config, Defined (Exactly))
import qualified Data.Holmes as Holmes
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Propagator (and', distinct, zipWith', (.$))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Signal = A | B | C | D | E | F | G deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable Signal

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq, Ord, Enum)

newtype SignalPattern = SignalPattern {_spSignals :: Set Signal} deriving (Show, Eq, Ord)

makeLenses ''SignalPattern

data Entry = Entry
  { _entrySignalPatterns :: [SignalPattern],
    _entryOutput :: [SignalPattern]
  }
  deriving (Show, Eq, Ord)

makeLenses ''Entry

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print (solvePart1 parsed)
  solvePart2 parsed >>= print

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

solvePart1 :: [Entry] -> Int
solvePart1 = lengthOf (folded . entryOutput . folded . filtered isDigit)

solvePart2 :: [Entry] -> IO Int
solvePart2 es = fmap (foldl' (+) 0 . catMaybes) $
  forConcurrently es $ \e -> do
    m <- investigate e
    let maybeSolution = fmap (Map.fromList . (`zip` [D0 ..]) . fmap (SignalPattern . unpack)) m
        y = fmap (toNumber . (\solution -> fmap (solution Map.!) (e ^. entryOutput))) maybeSolution
    pure y
  where
    unpack (Exactly x) = x

toNumber :: [Digit] -> Int
toNumber ds = read @Int $ concatMap (show . fromEnum) ds

investigate :: Entry -> IO (Maybe [Defined (Set Signal)])
investigate e =
  holmesConfig e `satisfying` \ds@[d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] ->
    and'
      [ distinct ds,
        --
        (== one) .$ d1,
        (== seven) .$ d7,
        (== four) .$ d4,
        (== eight) .$ d8,
        --
        d1 `hasDistinct` 2,
        d7 `hasDistinct` 3,
        d4 `hasDistinct` 4,
        d2 `hasDistinct` 5,
        d3 `hasDistinct` 5,
        d5 `hasDistinct` 5,
        d0 `hasDistinct` 6,
        d6 `hasDistinct` 6,
        d9 `hasDistinct` 6,
        d8 `hasDistinct` 7,
        --
        zipWith' Set.intersection d1 d7 `hasDistinct` 2,
        zipWith' Set.intersection d1 d4 `hasDistinct` 2,
        zipWith' Set.difference d6 d9 `hasDistinct` 1,
        zipWith' Set.intersection d1 d5 `hasDistinct` 1,
        zipWith' Set.intersection d6 d7 `hasDistinct` 2,
        zipWith' Set.intersection d2 d5 `hasDistinct` 3,
        zipWith' Set.intersection d2 d6 `hasDistinct` 4,
        zipWith' Set.intersection d3 d9 `hasDistinct` 5,
        zipWith' Set.intersection d7 d9 `hasDistinct` 3
      ]
  where
    [one, seven, four, eight] = map findS [2, 3, 4, 7]
    findS n = e ^?! entrySignalPatterns . folded . spSignals . filtered ((== n) . length)
    hasDistinct p n = ((== n) . Set.size) .$ p

holmesConfig :: Entry -> Config Holmes (Defined (Set Signal))
holmesConfig e = Holmes.from 10 (e ^. entrySignalPatterns & map (view spSignals))

parser :: Parsec Text () [Entry]
parser = Parsec.many1 (entryParser <* (void Parsec.newline <|> void Parsec.eof))

signalParser :: Parsec Text () Signal
signalParser =
  Parsec.choice
    [ A <$ Parsec.char 'a',
      B <$ Parsec.char 'b',
      C <$ Parsec.char 'c',
      D <$ Parsec.char 'd',
      E <$ Parsec.char 'e',
      F <$ Parsec.char 'f',
      G <$ Parsec.char 'g'
    ]

spParser :: Parsec Text () SignalPattern
spParser = SignalPattern . Set.fromList <$> Parsec.many1 signalParser

entryParser :: Parsec Text () Entry
entryParser = Entry <$> (sps 10 <* Parsec.string "| ") <*> sps 4
  where
    sps n = replicateM n (spParser <* Parsec.optional (Parsec.char ' '))

isDigit :: SignalPattern -> Bool
isDigit (SignalPattern ds) = length ds `elem` [2, 3, 4, 7]
