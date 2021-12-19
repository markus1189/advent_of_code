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
import Control.Monad (void, guard)
import Data.Functor (($>))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Linear.V3
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Linear.Matrix
import Debug.Trace

data Scanner = Scanner
  { _scannerId :: Int,
    _scannerBeacons :: [V3 Int]
  }
  deriving (Show, Eq, Ord)

makeLenses ''Scanner

data Instr = RotateX | RotateY | RotateZ deriving (Show, Eq, Ord)

showScanner :: Scanner -> String
showScanner s = intercalate "\n" $ ["--- scanner " <> show (s ^. scannerId) <> "---"] ++ map showV3 (s ^. scannerBeacons)
  where showV3 (V3 x y z) = show x <> "," <> show y <> "," <> show z

rotateX v = m !* v
  where m = V3 (V3 1 0 0) (V3 0 0 (-1)) (V3 0 1 0)
rotateY v = m !* v
  where m = V3 (V3 0 0 1) (V3 0 1 0) (V3 (-1) 0 0)
rotateZ v = m!* v
  where m = V3 (V3 0 (-1) 0) (V3 1 0 0) (V3 0 0 1)

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: [Scanner] -> _
solvePart1 (s1 : s2 : _) = _
solvePart1 _ = error ""

allScanners :: Scanner -> [Scanner]
allScanners s = map (`applyInstrScanner` s) instrs
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
    applyInstr (RotateX:is) v = applyInstr is (rotateX v)
    applyInstr (RotateY:is) v = applyInstr is (rotateY v)
    applyInstr (RotateZ:is) v = applyInstr is (rotateZ v)

    applyInstrScanner :: [Instr] -> Scanner -> Scanner
    applyInstrScanner is = scannerBeacons %~ map (applyInstr is)

solvePart2 :: [Scanner] -> _
solvePart2 _ = ()

--findProjection :: [V3 Int] -> [V3 Int] -> _
findProjection vv1 vv2 = do
  v1 <- vv1
  v2 <- vv2
  let v3 = v1 - v2
  guard $ allVectorsHit v3 vv1 vv2
  pure v3

--allVectorsHit :: V3 Int -> [V3 Int] -> [V3 Int] -> Bool
allVectorsHit tgt vv1 vv2 = all (\v1 -> any (\v2 -> v1 - v2 == tgt) vv2) vv1

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
