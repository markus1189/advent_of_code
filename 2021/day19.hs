{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad (guard, void)
import Data.Functor (($>))
import Data.Graph.Inductive (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import Data.List
import Data.List.Extra (nubOrd, nubOrdOn)
import qualified Data.Map.Lazy as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Tuple (swap)
import Debug.Trace
import Linear.Matrix
import Linear.V3
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Scanner = Scanner
  { _scannerId :: Int,
    _scannerBeacons :: [V3 Int]
  }
  deriving (Show, Eq, Ord)

makeLenses ''Scanner

data Instr = RotateX | RotateY | RotateZ deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  solvePart1 parsed
  pure ()

solvePart1 :: [Scanner] -> _
solvePart1 ss = do
  putStrLn "\n---------------------------------------- paths ----------------------------------------\n"
  print paths
  putStrLn "\n---------------------------------------- edgeToInstr ----------------------------------------\n"
  print edgeToInstr
  putStrLn "\n---------------------------------------- g ----------------------------------------\n"
  print g
  putStrLn "\n---------------------------------------- bases ----------------------------------------\n"
  print bases
  putStrLn "\n---------------------------------------- pathInstrs ----------------------------------------\n"
  print pathInstrs
  putStrLn "\n---------------------------------------- resultBases ----------------------------------------\n"
  print resultBases
  putStrLn "\n---------------------------------------- result ----------------------------------------\n"
  print result
  where
    f x y = find (isJust . view _2) (map (\(i, s') -> (i,) <$> findProjection (x ^. scannerBeacons) . view scannerBeacons $ s') (allScanners y)) >>= (\(is, m) -> ((x ^. scannerId, y ^. scannerId),is,) <$> m)

    nodes = map ((\i -> (i, i)) . view scannerId) ss
    edges = map ((\(x, y) -> (x, y, 1)) . view _1) g
    graph = mkGraph @Gr nodes edges
    path n = sp @Gr @Int n 0 graph
    paths = mapMaybe (\x -> (x ^. scannerId,) <$> (fmap (reverse . map swap . pairs) . path . view scannerId $ x)) ss

    pathInstrs = Map.fromList $ map (over _2 (foldl' (\acc e -> acc <> [edgeToInstr Map.! e]) [])) $ paths

    bases :: Map (Int, Int) (V3 Int)
    bases = Map.fromList $ map (view _1 &&& view _3) g

    edgeToInstr :: Map (Int, Int) [Instr]
    edgeToInstr = Map.fromList $ map (\(k, v, _) -> (k, v)) g

    relativePositions :: Map Int (V3 Int)
    relativePositions = Map.fromList $ map (\sid -> (sid, V3 0 0 0)) $ map (view scannerId) ss

    result = (\x -> (length x, x)) $ nubOrd $ concatMap (\(Scanner sid beacons) -> let (is,b) = resultBases Map.! sid in map (\beacon -> b + applyInstr is beacon) beacons) ss

    resultBases =
      Map.fromList $
        mapMaybe
          ( \(sid, es) ->
              fmap (sid,) $
                if null es
                  then Just ([], V3 0 0 0)
                  else
                    if length es == 1
                      then Just ([], bases Map.! head es)
                      else
                        Just $
                          foldl'
                            ( \(isAcc, acc) (b1, b2) ->
                                let bb2 = bases Map.! b1
                                    bb1 = bases Map.! b2
                                    bis = edgeToInstr Map.! b1
                                 in (isAcc <> bis, acc + bases Map.! b1 + applyInstr (edgeToInstr Map.! b1) (bases Map.! b2))
                            )
                            ([], V3 0 0 0)
                            $ pairs es
          )
          paths

    g =
      catMaybes $ do
        x <- ss
        y <- ss
        guard $ x /= y
        pure $ f x y
    scannerTransformations = Map.fromList $ map (\((f, t), is, b) -> (t, (is, b))) g
solvePart1 _ = error ""

pairs :: [a] -> [(a, a)]
pairs xs = xs `zip` tail xs

solve :: [((Int, Int), ([Instr], V3 Int, [V3 Int]))] -> _
solve = _
  where
    zeroToOne = _

allScanners :: Scanner -> [([Instr], Scanner)]
allScanners s = map (\is -> (is, applyInstrScanner is s)) instrs

instrs =
  [ [],
    [RotateX],
    [RotateX, RotateX],
    [RotateX, RotateX, RotateX],
    [RotateY],
    [RotateY, RotateY],
    [RotateY, RotateY, RotateY],
    [RotateZ],
    [RotateZ, RotateZ],
    [RotateZ, RotateZ, RotateZ],
    [RotateX, RotateZ],
    [RotateX, RotateZ, RotateZ],
    [RotateX, RotateZ, RotateZ, RotateZ],
    [RotateX, RotateX, RotateZ],
    -- [RotateX, RotateX, RotateZ, RotateZ],
    [RotateX, RotateX, RotateZ, RotateZ, RotateZ],
    [RotateX, RotateX, RotateX, RotateZ],
    [RotateX, RotateX, RotateX, RotateZ, RotateZ],
    [RotateX, RotateX, RotateX, RotateZ, RotateZ, RotateZ],
    [RotateY, RotateZ],
    [RotateY, RotateZ, RotateZ],
    [RotateY, RotateZ, RotateZ, RotateZ],
    -- [RotateY, RotateY, RotateZ],
    -- [RotateY, RotateY, RotateZ, RotateZ],
    -- [RotateY, RotateY, RotateZ, RotateZ, RotateZ],
    [RotateY, RotateY, RotateY, RotateZ],
    [RotateY, RotateY, RotateY, RotateZ, RotateZ],
    [RotateY, RotateY, RotateY, RotateZ, RotateZ, RotateZ]
  ]

applyInstr :: Num a => [Instr] -> V3 a -> V3 a
applyInstr [] v = v
applyInstr (RotateX : is) v = applyInstr is (rotateX v)
applyInstr (RotateY : is) v = applyInstr is (rotateY v)
applyInstr (RotateZ : is) v = applyInstr is (rotateZ v)

applyInstrScanner :: [Instr] -> Scanner -> Scanner
applyInstrScanner is = scannerBeacons %~ map (applyInstr is)

rotateX :: Num a => V3 a -> V3 a
rotateX v = m !* v
  where
    m = V3 (V3 1 0 0) (V3 0 0 (-1)) (V3 0 1 0)

rotateY :: Num a => V3 a -> V3 a
rotateY v = m !* v
  where
    m = V3 (V3 0 0 1) (V3 0 1 0) (V3 (-1) 0 0)

rotateZ :: Num a => V3 a -> V3 a
rotateZ v = m !* v
  where
    m = V3 (V3 0 (-1) 0) (V3 1 0 0) (V3 0 0 1)

solvePart2 :: [Scanner] -> _
solvePart2 _ = ()

findProjection :: (Eq a, Num a) => [a] -> [a] -> Maybe a
findProjection vv1 vv2 = listToMaybe $ do
  v1 <- vv1
  v2 <- vv2
  let v3 = v1 - v2
      hits = allVectorsHit v3 vv1 vv2
  guard $ (== 12) $ length $ take 12 hits
  pure v3

allVectorsHit tgt vv1 vv2 = filter (\v1 -> any (\v2 -> v1 - v2 == tgt) vv2) vv1

parser :: Parsec Text () _
parser = Parsec.many1 (scannerParser <* (void Parsec.newline <|> void Parsec.eof))

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left e -> error (show e)
    Right r -> r

scannerParser = Scanner <$> header <*> Parsec.many1 (coord <* (void Parsec.newline <|> void Parsec.eof))
  where
    header = Parsec.string "--- scanner " *> numberParser <* Parsec.string " ---" <* Parsec.newline
    coord = V3 <$> numberParser <*> (Parsec.char ',' *> numberParser) <*> (Parsec.char ',' *> numberParser)

numberParser :: Parsec Text () Int
numberParser = do
  sign <- Parsec.option 1 (Parsec.char '-' $> (-1))
  (* sign) . read @Int <$> Parsec.many1 Parsec.digit
