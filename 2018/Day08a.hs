module Day08a where

import Data.List.Split

type  Stack a = [a]

parseInput :: String -> [Int]
parseInput = map read . splitOn " "

sampleInput :: String
sampleInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
