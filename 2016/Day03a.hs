module Day03a where

import Text.Regex.PCRE
import Data.List

type Triangle = [Int]

main :: IO ()
main = do
  s <- readFile "input/Day03.txt"
  let triangles = map parseLine $ lines s
      validTriangles = filter isValid triangles
    in print $ length validTriangles

parseLine :: String -> Triangle
parseLine s = map read matches
  where
    matches = getAllTextMatches (s =~ "\\d+" :: AllTextMatches [] String)

isValid :: Triangle -> Bool
isValid t = all (\[a, b, c] -> a + b > c) perms
  where
    perms = permutations t
