{-# LANGUAGE  TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import  Control.Lens.TH  (makeLenses)
import  Control.Lens.Combinators
import  Control.Lens.Operators
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  let parsed = parseInput parser input
  print parsed
  print $ solvePart1 parsed
  print $ solvePart2 parsed

solvePart1 :: _ -> _
solvePart1 _ = ()

solvePart2 :: _ -> _
solvePart2 _ = ()

parser :: Parsec Text () _
parser = pure ()

parseInput :: Parsec Text () a -> Text -> a
parseInput p input =
  case Parsec.runParser p () "stdin" input of
    Left  e -> error (show e)
    Right r -> r
