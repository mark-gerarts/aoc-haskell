module Day04b where

import Data.List

main :: IO ()
main = do
  s <- readFile "input/Day04.txt"
  let phrases = lines s
      count = length $ filter isValid phrases
    in print count

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = sort s1 == sort s2

isValid :: String -> Bool
isValid s = isValid' passphraseWords
  where
    passphraseWords = words s
    isValid' [] = True
    isValid' (w:ws) | any (`isAnagram`w) ws = False
                    | otherwise = isValid' ws
