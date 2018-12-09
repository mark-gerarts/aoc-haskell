module Day02a where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ solution s

solution :: String -> String
solution s = show $ numberOf2s * numberOf3s
  where
    occurenceList = (map get2And3Occurrences . lines) s
    numberOf2s = foldl (\count (twos, _) -> if twos then count + 1 else count) 0 occurenceList
    numberOf3s = foldl (\count (_, threes) -> if threes then count + 1 else count) 0 occurenceList

sampleInput :: [String]
sampleInput =
  [
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab"
  ]

countOccurrences :: Char -> String -> Int
countOccurrences c = length . filter (==c)

get2And3Occurrences :: String -> (Bool, Bool)
get2And3Occurrences s = (isJust has2s, isJust has3s)
  where
    counts = map (`countOccurrences` s) (nub s)
    has2s = find (==2) counts
    has3s = find (==3) counts
