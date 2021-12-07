{-# LANGUAGE TypeApplications #-}

module Parsers (numberParser, commaSeperatedNumbersParser, parseInput) where

import Data.Text (Text)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit

commaSeperatedNumbersParser :: Parsec Text () [Int]
commaSeperatedNumbersParser =
  numberParser `Parsec.sepBy1` Parsec.char ','
    <* Parsec.many Parsec.newline
    <* Parsec.eof

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left  e -> error (show e)
    Right r -> r
