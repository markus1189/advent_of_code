{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Lens.Combinators ( Identity )
import Control.Lens.Operators ( (^?!) )
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Numeric (readHex)
import Text.Printf (printf)
import Control.Monad (replicateM)
import Numeric.Lens (binary)
import Data.List (foldl')

type Version = Int
type Type = Int

data Packet = LiteralValue Version Int | Operator Version OpType [Packet] deriving (Eq, Show, Ord)

data OpType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving (Eq, Show, Ord)

opTypeParser :: Int -> Parsec String () OpType
opTypeParser i = case i of
  0 -> pure Sum
  1 -> pure Product
  2 -> pure Minimum
  3 -> pure Maximum
  5 -> pure GreaterThan
  6 -> pure LessThan
  7 -> pure EqualTo
  _ -> fail $ "Unknown op type: " <> show i

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print parsed
  print $ solvePart1 parsed
  print $ solvePart2 parsed

versions :: Packet -> [Int]
versions (LiteralValue v _) = pure v
versions (Operator v _ ps) = v : concatMap versions ps

interpret :: Packet -> Int
interpret (LiteralValue _ i) = i
interpret (Operator _ Sum ps) = foldl' (+) 0 $ map interpret ps
interpret (Operator _ Product ps) = foldl' (*) 1 $ map interpret ps
interpret (Operator _ Minimum ps) = minimum $ map interpret ps
interpret (Operator _ Maximum ps) = maximum $ map interpret ps
interpret (Operator _ GreaterThan [p1,p2]) = if interpret p1 > interpret p2 then 1 else 0
interpret (Operator _ LessThan [p1,p2]) = if interpret p1 < interpret p2 then 1 else 0
interpret (Operator _ EqualTo [p1,p2]) = if interpret p1 == interpret p2 then 1 else 0

solvePart1 :: String -> Int
solvePart1 s = foldl' (+) 0 . versions $ parseInput packetParser s

solvePart2 :: String -> Int
solvePart2 s = interpret $ parseInput packetParser s

packetParser :: Parsec String () Packet
packetParser = do
  v <- packetVersion
  t <- packetTypeId
  case t of
    4 -> LiteralValue v <$> parseLiteral
    _ -> parseOperator v t

parseOperator :: Version -> Type -> Parsec.Parsec String () Packet
parseOperator v t = do
  opType <- opTypeParser t
  b <- (== '0') <$> bitP
  if b
    then do
      totalLengthInBits <- (^?! binary) <$> replicateM 15 bitP
      i <- Parsec.getInput
      Parsec.setInput (take totalLengthInBits i)
      ps <- Parsec.many1 packetParser
      Parsec.setInput (drop totalLengthInBits i)
      pure (Operator v opType ps)
    else do
      numberOfSubpackets <- (^?! binary) <$> replicateM 11 bitP
      ps <- replicateM numberOfSubpackets packetParser
      pure (Operator v opType ps)

parseLiteral :: Parsec String () Int
parseLiteral = (^?! binary) <$> parseGroups
  where
    parseGroups = do
      lastGroup <- (== '0') <$> bitP
      bs <- replicateM 4 bitP
      if lastGroup
        then pure bs
        else (bs <>) <$> parseGroups

toBinary :: String -> String
toBinary = concatMap (printf "%04b" . fst . head . readHex @Int . pure)

packetVersion :: Parsec String () Int
packetVersion = (^?! binary) <$> replicateM 3 bitP

packetTypeId :: Parsec String () Int
packetTypeId = packetVersion

bitP :: Parsec String () Char
bitP = Parsec.char '1' <|> Parsec.char '0'

parser :: Parsec Text () String
parser = toBinary <$> (Parsec.many1 Parsec.hexDigit <* Parsec.many Parsec.newline <* Parsec.eof)

parseInput :: Parsec.Stream s Identity t => Parsec s () p -> s -> p
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left  e -> error (show e)
    Right r -> r
