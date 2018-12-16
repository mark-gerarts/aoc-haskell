module Day02b where

import Prelude hiding (Left, Right)
import Data.List

data Instruction = Up | Right | Down | Left deriving (Show, Eq)

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ getCode (parseInput s) '5' []

getCode :: [[Instruction]] -> Char -> [Char] -> [Char]
getCode [] _ code = code
getCode (i:is) currentDigit code = getCode is nextDigit (code ++ [nextDigit])
  where
    nextDigit = getDigit i currentDigit

getDigit :: [Instruction] -> Char -> Char
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

move :: Instruction -> Char -> Char
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
    upDownPairs = [('1', '3'), ('2', '6'), ('3', '7'), ('4', '8'),
                   ('6', 'A'), ('7', 'B'), ('8', 'C'), ('B', 'D')]
    leftRightPairs = [('5', '6'), ('2', '3'), ('6', '7'), ('A', 'B'),
                      ('3', '4'), ('7', '8'), ('B', 'C'), ('8', '9')]
