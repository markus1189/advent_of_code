{-# LANGUAGE  TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

import  Control.Lens.TH  (makeLenses)
import  Control.Lens.Combinators
import  Control.Lens.Operators
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Control.Monad (void, replicateM, guard)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Linear (V2)
import Linear.V2 (V2(V2))
import Numeric.Lens (binary)
import Data.List.Extra (nubOrd)
import Debug.Trace

data Tile = Dark | Light deriving (Eq, Ord)

instance Show Tile where
  show Light = "#"
  show Dark = "."

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: _ -> _
solvePart1 (ieas,g) = Map.size $ Map.filter (==Light) $ (!!1) $ iterate (enhance (ieas !! 0) ieas . enhance Dark ieas) g

solvePart2 :: _ -> _
solvePart2 (ieas, g) = Map.size $ Map.filter (==Light) $ (!!25) $ iterate (enhance (ieas !! 0) ieas . enhance Dark ieas) g

enhance def ieas g = Map.map (\ts -> if Light `elem` ts then Light else Dark) $ Map.fromListWith (<>) $ map (\p -> (p, [next p])) $ nubOrd $ concatMap neighNeighbors $ Map.keys g'
  where
    g' = g
    next = newPixelValue def ieas g'

padMap :: Map (V2 Int) Tile -> Map (V2 Int) Tile
padMap m = _

neighNeighbors :: V2 Int -> [V2 Int]
neighNeighbors p = nubOrd $ underFocus p

newPixelValue :: Tile -> [Tile] -> Map (V2 Int) Tile -> V2 Int -> Tile
newPixelValue def ieas g p = ieas ^?! ix (focusToBinary def g p)

underFocus :: (Traversable f, Applicative f, Ord (f a), Num a, Num (f a)) => f a -> [(f a)]
underFocus p = do
  delta <- sequence (pure [-1, 0, 1])
  pure $ p + delta

focusToBinary def g p = (^?! binary) $ map toBit $ map (\k -> lookupPixel def g k) $ underFocus p

toBit Dark = '0'
toBit Light = '1'

lookupPixel :: Tile -> Map (V2 Int) Tile -> (V2 Int) -> Tile
lookupPixel def m k = fromMaybe def $ Map.lookup k m

parser :: Parsec Text () _
parser = (,) <$> ieasParser <* Parsec.newline <* Parsec.newline <*> (toMap <$> coordParser)

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

ieasParser :: Parsec Text () [Tile]
ieasParser = replicateM 512 tile

coordParser :: Parsec.ParsecT Text u Identity [[Tile]]
coordParser = Parsec.many1 (Parsec.many1 tile <* (void Parsec.eof <|> void Parsec.newline))

tile :: Parsec.ParsecT Text u Identity Tile
tile = (Parsec.char '#' $> Light) <|> (Parsec.char '.' $> Dark)

toMap :: [[a]] -> Map (V2 Int) a
toMap = Map.fromList . map (\((x, y), v) -> (V2 x y, v)) . toListOf ((itraversed <.> itraversed) . withIndex)
