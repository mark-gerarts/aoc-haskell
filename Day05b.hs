module Day05b where

import Data.Char

main :: IO ()
main = do
  s <- readFile "input/Day05.txt"
  print $ minimum $ getPerformanceMap $ init s

getPerformanceMap :: String -> [(Int, Char)]
getPerformanceMap s = map (\c -> (measure c, c)) ['a'..'z']
  where
    measure c = length $ reduceUntilFinished $ removeChar c s

removeChar :: Char -> String -> String
removeChar c = filter (caseInsensitiveNottEquals c)
  where
    caseInsensitiveNottEquals a b = toUpper a /= toUpper b

reduceUntilFinished :: String -> String
reduceUntilFinished s
  | isFinished s = s
  | otherwise = reduceUntilFinished (reduce s)

isFinished :: String -> Bool
isFinished s = reduce s == s

reduce :: String -> String
reduce [] = ""
reduce [c] = [c]
reduce (a:b:cs)
  | isOpposite a b = reduce cs
  | otherwise = a : reduce (b : cs)

isOpposite :: Char -> Char -> Bool
isOpposite c c'
  | isLower c && isUpper c' = toUpper c == c'
  | isUpper c && isLower c' = c == toUpper c'
  | otherwise = False

sampleInput :: String
sampleInput = "dabAcCaCBAcCcaDA"
