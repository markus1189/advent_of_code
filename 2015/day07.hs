{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Word (Word16)
import Data.Bits
import           Control.Applicative ((<|>))
import           Data.Foldable (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec, (<?>))
import qualified Text.Parsec as Parsec

data Wire = Signal Word16
          | SigVar String
          | WireOr Wire Wire
          | WireAnd Wire Wire
          | WireNot Wire
          | WireRShift Wire Int
          | WireLShift Wire Int
          deriving (Show, Eq, Ord)

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

main :: IO ()
main = do
  input <- TIO.getContents
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: [(String, Wire)] -> Maybe Word16
solvePart1 = Map.lookup "a" . loeb . foldl' go Map.empty
  where go acc (k,w) = Map.insert k (eval w) acc

solvePart2 :: [(String, Wire)] -> Maybe Word16
solvePart2 input = solvePart1 (input ++ [("b",Signal signalA)])
  where Just signalA = solvePart1 input

eval :: Wire -> Map String Word16 -> Word16
eval (Signal i) _  = i
eval (SigVar v) m  = m Map.! v
eval (WireOr lhs rhs) m  = eval lhs m .|. eval rhs m
eval (WireAnd lhs rhs) m  = eval lhs m .&. eval rhs m
eval (WireNot e) m  = complement (eval e m)
eval (WireRShift w n) m  = shiftR (eval w m) n
eval (WireLShift w n) m  = shiftL (eval w m) n

parseInput :: Text -> [(String, Wire)]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [(String, Wire)]
parser = Parsec.many1 (lineParser <* Parsec.newline) <* Parsec.eof

lineParser :: Parsec Text () (String, Wire)
lineParser = flip (,) <$> p <* Parsec.string " -> " <*> Parsec.many1 Parsec.lower
  where p = Parsec.choice [ Parsec.try notParser
                          , Parsec.try rshiftParser
                          , Parsec.try lshiftParser
                          , Parsec.try andParser
                          , Parsec.try orParser
                          , Parsec.try signalParser
                          , Parsec.try sigVarParser
                          ]

numberParser :: (Read a, Num a) => Parsec Text () a
numberParser = (read <$> Parsec.many1 Parsec.digit) <?> "number"

sigVarParser :: Parsec Text () Wire
sigVarParser = (SigVar <$> Parsec.many1 Parsec.lower) <?> "signal variable"

signalParser :: Parsec Text () Wire
signalParser = (Signal . read @Word16 <$> Parsec.many1 Parsec.digit) <?> "raw signal"

andParser :: Parsec Text () Wire
andParser = (WireAnd <$> (p <* Parsec.string " AND ") <*> p) <?> "and expression"
  where p = sigVarParser <|> signalParser

orParser :: Parsec Text () Wire
orParser = (WireOr <$> (sigLeafParser <* Parsec.string " OR ") <*> sigLeafParser) <?> "or expression"

sigLeafParser :: Parsec Text () Wire
sigLeafParser = sigVarParser <|> signalParser

notParser :: Parsec Text () Wire
notParser = (WireNot <$> (Parsec.string "NOT " *> sigLeafParser)) <?> "not expression"

rshiftParser :: Parsec Text () Wire
rshiftParser = (WireRShift <$> (sigLeafParser <* Parsec.string " RSHIFT ") <*> numberParser) <?> "rshift expression"

lshiftParser :: Parsec Text () Wire
lshiftParser = (WireLShift <$> (sigLeafParser <* Parsec.string " LSHIFT ") <*> numberParser) <?> "lshift expression"
