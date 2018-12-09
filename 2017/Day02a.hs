module Day02a where

import Data.List.Split

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ solve s

solve :: String -> Int
solve = checkSum . map parseLine . lines

checkSum :: [[Int]] -> Int
checkSum = sum . map rowDiff

rowDiff :: [Int] -> Int
rowDiff xs = maximum xs - minimum xs

parseLine :: String -> [Int]
parseLine = map read . splitOn "\t"
