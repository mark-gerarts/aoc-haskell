module Day07a where

import Text.Regex.PCRE

type IPv7Address = ([String], [String])

main :: IO ()
main = do
  s <- readFile "input/Day07.txt"
  print $ solve s

solve :: String -> Int
solve = length . filter supportsTLS . map parse . lines

parse :: String -> IPv7Address
parse s = (hypernetSequences, parts)
  where
    hypernetSequences = getAllTextMatches (s =~ "(?<=\\[).*?(?=\\])" :: AllTextMatches [] String)
    parts = getAllTextMatches (s =~ "(^.*?(?=\\[)|(?<=\\]).*?(?=\\[)|(?<=\\]).*?$)" :: AllTextMatches [] String)

containsAbba :: String -> Bool
containsAbba [] = False
containsAbba (a:b:c:d:xs) | a == d && b == c && a /= b = True
                          | otherwise = containsAbba (b:c:d:xs)
containsAbba _ = False

supportsTLS :: IPv7Address -> Bool
supportsTLS (hypernetSequences, parts) =
  any containsAbba parts
  && all (not . containsAbba) hypernetSequences
