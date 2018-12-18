module Day04b where

import Data.List
import Text.Regex.PCRE
import Data.Ord
import Data.Char

data Room = Room { _string :: String
                 , _id :: Int
                 , _checksum :: String
                 } deriving (Show, Eq)

main :: IO ()
main = do
  s <- readFile "input/Day04.txt"
  print $ solve s

solve :: String -> [String]
solve input = filter (isInfixOf "North") sentences
  where
    validRooms = (filter isValid . map parse . lines) input
    sentences = map decryptRoom validRooms

decryptRoom :: Room -> String
decryptRoom (Room s id _) = decrypt id s

decrypt :: Int -> String -> String
decrypt n = map (decryptChar n)

decryptChar :: Int -> Char -> Char
decryptChar _ '-' = ' '
decryptChar n c = ['a'..'z'] !! (((ord c - ord 'a') + n) `mod` 26)

isValid :: Room -> Bool
isValid (Room s _ checksum) = expectedChecksum == checksum
  where
    charCounts = filter (\cc -> fst cc /= '-') $ getCharCounts s
    sortedCharCounts = sortOn (Down . snd) charCounts
    expectedChecksum = map fst $ take 5 sortedCharCounts

getCharCounts :: String -> [(Char, Int)]
getCharCounts s = [(c, n) | c <- sort (nub s), let n = length (filter (==c) s)]

parse :: String -> Room
parse s = Room string (read id) checksum
  where
    string = s =~ ".*?(?=\\d)" :: String
    id = s =~ "\\d+" :: String
    checksum = s =~ "(?<=\\[).*(?=\\])" :: String
