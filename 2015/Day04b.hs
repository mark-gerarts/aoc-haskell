module Day04b where

import Data.Hex
import Crypto.Hash.MD5
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BS

md5 :: String -> String
md5 = C8.unpack . hex . hash . C8.pack

isSpecial :: String -> Bool
isSpecial s = "000000" `isPrefixOf` s

getKey :: String -> Int
getKey s = go 1
  where
    go i | isSpecial (md5 (s ++ (show i))) = i
         | otherwise = go (i + 1)
