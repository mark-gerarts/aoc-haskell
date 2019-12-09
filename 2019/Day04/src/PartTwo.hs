module PartTwo where

import Data.List
import Data.Char

main :: IO ()
main = print $ getNumberOfPasswords 134792 675810

getNumberOfPasswords :: Int -> Int -> Int
getNumberOfPasswords min max = length $ filter isValid $ map toDigits [min..max]

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

isValid :: [Int] -> Bool
isValid xs = increasing && (twoAdjacent xs)
    where
        increasing = sort xs == xs

-- We can cheat this because we know the list has to be increasing.
twoAdjacent :: [Int] -> Bool
twoAdjacent xs = 2 `elem` occurences
    where
        occurences = map (\x -> length $ filter (==x) xs) [1..9]
