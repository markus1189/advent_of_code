{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Foldable (foldl')
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Numeric.Natural
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Present = Present Natural Natural Natural deriving (Show, Eq)

newtype SquareFeet = SquareFeet Natural deriving newtype (Show, Eq, Num)

main :: IO ()
main = do
  input <- TIO.getContents
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: [Present] -> SquareFeet
solvePart1 = foldl' (\acc p -> acc + surfaceArea p + slack p) 0

solvePart2 :: [Present] -> Natural
solvePart2 = foldl' (\acc p -> acc + ribbon p) 0

ribbon :: Present -> Natural
ribbon p = volume p + minimum (perimeters p)

surfaceArea :: Present -> SquareFeet
surfaceArea (Present l w h) = SquareFeet $ 2*l*w + 2*w*h + 2*h*l

slack :: Present -> SquareFeet
slack (Present l w h) = SquareFeet . minimum $ [l*w, w*h, h*l]

volume :: Present -> Natural
volume (Present l w h) = l*w*h

perimeters :: Present -> [Natural]
perimeters (Present l w h) = [2*(l+w), 2*(w+h), 2*(h+l)]

parseInput :: Text -> [Present]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Present]
parser = Parsec.many1 (presentParser <* Parsec.newline) <* Parsec.eof

numberParser :: (Read a, Num a) => Parsec Text () a
numberParser = read <$> Parsec.many1 Parsec.digit

presentParser :: Parsec Text () Present
presentParser = Present <$> numberParser <* Parsec.char 'x'
                        <*> numberParser <* Parsec.char 'x'
                        <*> numberParser
