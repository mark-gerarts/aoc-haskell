module Day05a where

import Data.Hex
import Crypto.Hash.MD5
import Data.Char
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BS

solve :: String -> String
solve input = reverse $ go 0 ""
  where
    go i cs | length cs == 8 = cs
            | otherwise = go (nextI + 1) (c : cs)
              where
                (nextI, c) = nextHash input i

nextHash :: String -> Int -> (Int, Char)
nextHash s i = case getPasswordChar (md5 si) of
                 Just c -> (i, c)
                 Nothing -> nextHash s (i + 1)
  where
    si = s ++ (show i)

md5 :: String -> String
md5 = C8.unpack . hex . hash . C8.pack

getPasswordChar :: String -> Maybe Char
getPasswordChar cs
  | length cs < 6 = Nothing
  | take 5 cs == replicate 5 '0' = Just $ cs !! 5
  | otherwise = Nothing
