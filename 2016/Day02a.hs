module Day02a where

import Prelude hiding (Left, Right)
import Data.List

data Instruction = Up | Right | Down | Left deriving (Show, Eq)

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ getCode (parseInput s) 5 []

getCode :: [[Instruction]] -> Int -> [Int] -> [Int]
getCode [] _ code = code
getCode (i:is) currentDigit code = getCode is nextDigit (code ++ [nextDigit])
  where
    nextDigit = getDigit i currentDigit

getDigit :: [Instruction] -> Int -> Int
getDigit [] currentDigit = currentDigit
getDigit (i:is) currentDigit = getDigit is (move i currentDigit)

parseInput :: String -> [[Instruction]]
parseInput = map (map parseInstruction) . lines

parseInstruction :: Char -> Instruction
parseInstruction 'U' = Up
parseInstruction 'R' = Right
parseInstruction 'D' = Down
parseInstruction 'L' = Left
parseInstruction _ = error "No Parse"

move :: Instruction -> Int -> Int
move i x = case i of
  Left -> case find ((==x) . snd) leftRightPairs of
            Just pair -> fst pair
            Nothing -> x
  Right -> case find ((==x) . fst) leftRightPairs of
             Just pair -> snd pair
             Nothing -> x
  Up -> case find ((==x) . snd) upDownPairs of
          Just pair -> fst pair
          Nothing -> x
  Down -> case find ((==x) . fst) upDownPairs of
            Just pair -> snd pair
            Nothing -> x
  where
    upDownPairs = zip [1..6] [4..9]
    leftRightPairs = [(1, 2), (4, 5), (7, 8), (2, 3), (5, 6), (8, 9)]
