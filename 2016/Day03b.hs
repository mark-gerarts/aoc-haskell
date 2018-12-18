module Day03b where

import Text.Regex.PCRE
import Data.List

type Triangle = [Int]

main :: IO ()
main = do
  s <- readFile "input/Day03.txt"
  let triangles = extractTriangles $ map parseLine $ lines s
      validTriangles = filter isValid triangles
    in print $ length validTriangles

parseLine :: String -> [Int]
parseLine s = map read matches
  where
    matches = getAllTextMatches (s =~ "\\d+" :: AllTextMatches [] String)

extractTriangles :: [[Int]] -> [Triangle]
extractTriangles input = go input []
  where
    go [] ts = ts
    go (a:b:c:xs) ts = go xs (ts ++ triangles)
      where
        rows = [a,b,c]
        triangles = [map head rows] ++ [map (!!1) rows] ++ [map (!!2) rows]
    go _ _ = error "Invalid number of rows"

isValid :: Triangle -> Bool
isValid t = all (\[a, b, c] -> a + b > c) perms
  where
    perms = permutations t
