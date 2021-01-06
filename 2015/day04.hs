{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.List (find)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16

main :: IO ()
main = do
  print . solve $ "00000"
  print . solve $ "000000"

solve :: BS.ByteString -> Maybe (Int, BS.ByteString)
solve prefix = find ((prefix `BS.isPrefixOf`) . snd)
             . map @Int ((,) <$> id <*> hashWithSecret)
             $ [1..]

hashWithSecret :: Int -> BS.ByteString
hashWithSecret = B16.encode . MD5.hash . ("yzbqklnj" <>) . BS.pack . show
