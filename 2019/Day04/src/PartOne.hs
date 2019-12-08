module Main where

import Data.List
import Data.Char

main :: IO ()
main = print $ getNumberOfPasswords 134792 675810

getNumberOfPasswords :: Int -> Int -> Int
getNumberOfPasswords min max = length $ filter isValid $ map toDigits [min..max]

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

isValid :: [Int] -> Bool
isValid xs = increasing && twoAdjacent
  where
    increasing = sort xs == xs
    twoAdjacent = any (\(x,y) -> x == y) $ zip xs (tail xs)
