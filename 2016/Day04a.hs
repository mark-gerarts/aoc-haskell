module Day04a where

import Data.List
import Text.Regex.PCRE
import Data.Ord

data Room = Room { _string :: String
                 , _id :: Int
                 , _checksum :: String
                 } deriving (Show, Eq)

main :: IO ()
main = do
  s <- readFile "input/Day04.txt"
  print $ solve s

solve :: String -> Int
solve = sum . map _id . filter isValid . map parse . lines

isValid :: Room -> Bool
isValid (Room s _ checksum) = expectedChecksum == checksum
  where
    sortedCharCounts = sortOn (Down . snd) $ getCharCounts s
    expectedChecksum = map fst $ take 5 sortedCharCounts

getCharCounts :: String -> [(Char, Int)]
getCharCounts s = [(c, n) | c <- sort (nub s), let n = length (filter (==c) s)]

parse :: String -> Room
parse s = Room string (read id) checksum
  where
    stringWithDashes = s =~ ".*?(?=\\d)" :: String
    string = filter (/='-')  stringWithDashes
    id = s =~ "\\d+" :: String
    checksum = s =~ "(?<=\\[).*(?=\\])" :: String
