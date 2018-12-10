module Day04a where

import Data.List

main :: IO ()
main = do
  s <- readFile "input/Day04.txt"
  let phrases = lines s
      count = length $ filter isValid phrases
    in print count

isValid :: String -> Bool
isValid s = passphraseWords == nub passphraseWords
  where
    passphraseWords = words s
