module Day02b where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ solve $ lines s

solve :: [String] -> String
solve xs = a `intersect` b
  where
    (a, b) = getMatchingIds xs

differsByOne :: Eq a => [a] -> [a] -> Int -> Bool
differsByOne s1 s2 numberOfDiffers
  | numberOfDiffers > 1 = False
  | s1 == s2 && null s1 = True
  | otherwise = differsByOne (tail s1) (tail s2) newInc
  where
    shouldInc = head s1 /= head s2
    newInc = if shouldInc then numberOfDiffers + 1 else numberOfDiffers

getMatchingIds :: [String] -> (String, String)
getMatchingIds = fromJust . find (\(x,y) -> differsByOne x y 0) . toPairs

toPairs :: [String] -> [(String, String)]
toPairs xs = xs >>= (\x -> zip (repeat x) (filter (/=x) xs))
