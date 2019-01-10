module Day05b where

import Data.List

main = do
  s <- readFile "input/Day05.txt"
  print $ length $ filter isValid $ lines s

rule1 :: String -> Bool
rule1 = not . null . getDoubles . toPairs

toPairs :: [a]  -> [[a]]
toPairs s = [[x, y] | (x, y) <- zip s (tail s)]

getDoubles :: Eq a => [a] -> [a]
getDoubles xs = nub $ filter containsAtLeastTwice xs
  where
    containsAtLeastTwice x = length (filter (==x) xs) > 1

rule2 :: String -> Bool
rule2 (a:b:c:cs) | a == c = True
                 | otherwise = rule2 (b:c:cs)
rule2 _ = False

isValid :: String -> Bool
isValid s = rule1 s && rule2 s
