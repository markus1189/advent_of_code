{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (void)
import Data.Graph (Graph, graphFromEdges)
import qualified Data.Graph as Graph
import Data.List (foldl', sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Tuple (swap)
import qualified Math.Geometry.Grid as Grid
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import qualified Math.Geometry.GridMap as GridMap
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [[Int]] -> Int
solvePart1 xss =
  foldl' (+) 0
    . map ((+ 1) . snd)
    $ lowPoints (mkGridMap xss)

solvePart2 :: [[Int]] -> Int
solvePart2 xss =
  foldl' (*) 1
    . take 3
    . sortOn negate
    . map length
    . Graph.dfs g
    . mapMaybe (f . fst)
    $ lowPoints gm
  where
    gm = mkGridMap xss
    (g, _, f) = mkGraph gm

lowPoints :: LGridMap RectSquareGrid Int -> [((Int, Int), Int)]
lowPoints gm =
  GridMap.toList $
    GridMap.filterWithKey
      ( \k v ->
          (v <)
            . minimum
            . map (gm GridMap.!)
            . Grid.neighbours (GridMap.toGrid gm)
            $ k
      )
      gm

mkGraph :: LGridMap RectSquareGrid Int -> (Graph, Graph.Vertex -> (Int, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Graph.Vertex)
mkGraph gm = graphFromEdges converted
  where
    discardTo9 = filter ((/= 9) . (gm GridMap.!))
    converted = (\(h, c) -> (h, c, nbs c)) . swap <$> GridMap.toList gm
      where
        nbs c = discardTo9 $ Grid.neighbours (GridMap.toGrid gm) c

mkGridMap :: [[v]] -> LGridMap RectSquareGrid v
mkGridMap xss = lazyGridMapIndexed (rectSquareGrid (length xss) (length (head xss))) ixed
  where
    ixed = xss ^.. (itraversed <.> itraversed) . withIndex

parser :: Parsec Text () [[Int]]
parser = Parsec.many1 (Parsec.many1 (read @Int . pure <$> Parsec.digit) <* (void Parsec.newline <|> void Parsec.eof))

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r
