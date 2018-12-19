module Day07a where

import Text.Regex.PCRE

type IPv7Address = (String, String, String)

main :: IO ()
main = do
  s <- readFile "input/Day07.txt"
  print $ solve s

solve :: String -> Int
solve = length . filter supportsTLS . map parse . lines

parse :: String -> IPv7Address
parse s = (partA, hypernetSequence, partB)
  where
    hypernetSequence = s =~ "(?<=\\[).*(?=\\])" :: String
    partA = s =~ ".*(?=\\[)" :: String
    partB = s =~ "(?<=\\]).*" :: String

containsAbba :: String -> Bool
containsAbba [] = False
containsAbba (a:b:c:d:xs) | a == d && b == c && a /= b = True
                          | otherwise = containsAbba (b:c:d:xs)
containsAbba _ = False

supportsTLS :: IPv7Address -> Bool
supportsTLS (partA, hypernetSequence, partB) =
  (containsAbba partA || containsAbba partB)
  && (not . containsAbba) hypernetSequence
