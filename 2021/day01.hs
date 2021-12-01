{-# LANGUAGE TypeApplications #-}

import Data.Foldable (Foldable (foldl'))
import Data.Functor.Identity (Identity)
import Data.List (tails, transpose)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec

data Change = Inc | Dec | Unknown deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser (numbers <* Parsec.eof) () "stdin" input of
    Left e -> error (show e)
    Right inputs -> do
      print $ handleInput1 inputs
      print $ handleInput2 inputs

numbers :: Parsec.ParsecT T.Text u Identity [Int]
numbers = Parsec.many1 (read @Int <$> Parsec.many1 Parsec.digit <* Parsec.newline)

handleInput1 :: [Int] -> Int
handleInput1 = length . filter (== Inc) . fmap snd . catMaybes . scanl step Nothing
  where
    step Nothing cur = Just (cur, Unknown)
    step (Just (prev, _)) cur = Just (cur, if cur > prev then Inc else Dec)

handleInput2 :: [Int] -> Int
handleInput2 inputs = handleInput1 $ foldl' (+) 0 <$> filter ((== 3) . length) (windows 3 inputs)

windows :: Int -> [a] -> [[a]]
windows n xs = transpose (take n (tails xs))
