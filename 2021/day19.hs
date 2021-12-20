{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad (guard, void)
import Data.Functor (($>))
import Data.List
import Data.List.Extra (nubOrdOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
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
  traverse print $ solvePart1 parsed
  pure ()
  -- print relative
  -- print $ solvePart2 parsed

solutionPart1 =
  [ ((0, 1), [RotateY, RotateY], (V3 68 (-1246) (-43), [V3 404 (-588) (-901), V3 528 (-643) 409, V3 390 (-675) (-793), V3 (-537) (-823) (-458), V3 (-485) (-357) 347, V3 (-345) (-311) 381, V3 (-661) (-816) (-575), V3 (-618) (-824) (-621), V3 (-447) (-329) 318, V3 544 (-627) (-890), V3 423 (-701) 434, V3 459 (-707) 401])),
    ((1, 3), [], (V3 160 (-1134) (-23), [V3 (-340) (-569) (-846), V3 567 (-361) 727, V3 669 (-402) 600, V3 (-500) (-761) 534, V3 (-466) (-666) (-811), V3 (-429) (-592) 574, V3 703 (-491) (-529), V3 (-328) (-685) 520, V3 586 (-435) 557, V3 (-364) (-763) (-893), V3 807 (-499) (-711), V3 755 (-354) (-619)])),
    ((1, 4), [RotateY, RotateZ, RotateZ, RotateZ], (V3 88 113 (-1104), [V3 515 917 (-361), V3 (-340) (-569) (-846), V3 (-460) 603 (-452), V3 (-466) (-666) (-811), V3 (-355) 545 (-477), V3 703 (-491) (-529), V3 413 935 (-424), V3 (-391) 539 (-444), V3 (-364) (-763) (-893), V3 807 (-499) (-711), V3 755 (-354) (-619), V3 553 889 (-390)])),
    ((2, 4), [RotateX, RotateX, RotateZ], (V3 1125 (-168) 72, [V3 649 640 665, V3 682 (-795) 504, V3 500 723 (-460), V3 609 671 (-379), V3 697 (-426) (-610), V3 578 704 681, V3 493 664 (-388), V3 571 (-461) (-707), V3 646 (-828) 498, V3 640 759 510, V3 673 (-379) (-804), V3 577 (-820) 562]))
  ]

relative = m
  where m =  Map.fromList [ (1, V3 68 (-1246) (-43)),
                            (2, m Map.! 4 + applyInstr (therotations Map.! 4) (V3 1125 (-168) 72)),
                            (3, m Map.! 1 + applyInstr (therotations Map.! 1) (V3 160 (-1134) (-23))),
                            (4, m Map.! 1 + applyInstr (therotations Map.! 1) (V3 88 113 (-1104)))
             ]
therotations = Map.fromList [ (1,[RotateY, RotateY])
                  , (2,[RotateX, RotateX, RotateZ] ++ therotations Map.! 4)
                  , (3,therotations Map.! 1 ++ [])
                  , (4,therotations Map.! 1 ++ [RotateY, RotateZ, RotateZ, RotateZ])
                  ]

theSolutionP1 = Map.fromList [(1,V3 68 (-1246) (-43)),(2,V3 (-188) (-1061) 2186),(3,V3 (-92) (-2380) (-20)),(4,V3 (-20) (-1133) 1061)]


solvePart1 :: [Scanner] -> _
solvePart1 ss =
  g
  -- scannerTransformations
  -- map (\(Scanner sid beacons) -> map ((solution' Map.! sid +)) beacons) ss
  where
    solution' = Map.insert 0 (V3 0 0 0) theSolutionP1
    therotations' = Map.insert 0 [] therotations
    f x y = find (isJust . view _2) (map (\(i, s') -> (i,) <$> findProjection (x ^. scannerBeacons) . view scannerBeacons $ s') (allScanners y)) >>= (\(is, m) -> ((x ^. scannerId, y ^. scannerId),is,) <$> m)
    g = catMaybes $ do
      x <- ss
      y <- filter (\s -> s ^. scannerId > x ^. scannerId) ss
      pure $ f x y
    scannerTransformations = Map.fromList $ map (\((f,t),is,b) -> (t,(is,b))) g
solvePart1 _ = error ""

solve :: [((Int, Int), ([Instr], V3 Int, [V3 Int]))] -> _
solve = _
  where
    zeroToOne = _

allScanners :: Scanner -> [([Instr], Scanner)]
allScanners s = map (\is -> (is, applyInstrScanner is s)) instrs
  where

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
