{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Applicative ((<|>))
import           Control.Monad (replicateM)
import           Data.Foldable (foldl')
import           Data.Function (on)
import           Data.Monoid (Sum(..))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Expr = EChar Char
          | EBackslash
          | EQuote
          | EHex String
          deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print  . parseInput $ input
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: [[Expr]] -> Int
solvePart1 = uncurry ((-) `on` getSum) . foldMap countChars

solvePart2 :: [[Expr]] -> Int
solvePart2 = uncurry (flip (-) `on` getSum) . foldMap encodeChars

countChars :: [Expr] -> (Sum Int, Sum Int)
countChars = foldl' go (2, 0)
  where go (!codeChars, !memoChars) (EChar _) = (codeChars + 1, memoChars + 1)
        go (!codeChars, !memoChars) EBackslash = (codeChars + 2, memoChars + 1)
        go (!codeChars, !memoChars) EQuote = (codeChars + 2, memoChars + 1)
        go (!codeChars, !memoChars) (EHex _) = (codeChars + 4, memoChars + 1)

encodeChars :: [Expr] -> (Sum Int, Sum Int)
encodeChars = foldl' go (2,6)
  where go (!codeChars, !escChars) (EChar _) = (codeChars + 1, escChars + 1)
        go (!codeChars, !escChars) EBackslash = (codeChars + 2, escChars + 4)
        go (!codeChars, !escChars) EQuote = (codeChars + 2, escChars + 4)
        go (!codeChars, !escChars) (EHex _) = (codeChars + 4, escChars + 5)

parseInput :: Text -> [[Expr]]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [[Expr]]
parser = Parsec.many1 (Parsec.between (Parsec.char '"')
                       (Parsec.char '"')
                       (Parsec.many $ escapedParser <|> EChar <$> Parsec.lower) <* Parsec.newline) <* Parsec.eof

escapedParser :: Parsec Text () Expr
escapedParser = Parsec.char '\\' *> Parsec.choice [ EBackslash <$ Parsec.char '\\'
                                                  , EQuote <$ Parsec.char '"'
                                                  , hexParser]
  where hexParser = Parsec.char 'x' *> (EHex <$> replicateM 2 (Parsec.digit <|> Parsec.oneOf ['a'..'f']))
