module Main where

import Data.List.Split
import qualified Data.Vector as V
import Data.List

type Instructions = V.Vector Int

type IntCode = Int

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructionList = parseInstructionList input
    in  print $ findNounAndVerbFor 19690720 instructionList

findNounAndVerbFor :: Int -> Instructions -> (Int, Int)
findNounAndVerbFor x is =
  case match of
    Just (_, nv) -> nv
    _ -> error "No match found"
  where
    inputs = [(n, v) | n <- [0..99], v <- [0..99]]
    results = map (\(n, v) -> (executeWithNounAndVerb n v is, (n, v))) inputs
    match = find (\(res, _) -> if res == x then True else False) results

executeWithNounAndVerb :: Int -> Int -> Instructions -> Int
executeWithNounAndVerb noun verb is = (V.!) executedList 0
  where
    replacedList = (V.//) is [(1, noun), (2, verb)]
    executedList = eval 0 replacedList

parseInstructionList :: String -> Instructions
parseInstructionList = V.fromList . map read . splitOn ","

eval :: Int -> Instructions -> Instructions
eval pos is =
  case (V.!) is pos of
    1 -> eval (pos + 4) $ output (leftOperand + rightOperand)
    2 -> eval (pos + 4) $ output (leftOperand * rightOperand)
    99 -> is
    _ -> error "Trying to use a value as an instruction"
  where
    getOffsetValue i = (V.!) is ((V.!) is (pos + i))
    leftOperand = getOffsetValue 1
    rightOperand = getOffsetValue 2
    outputIndex = (V.!) is (pos + 3)
    output x = (V.//) is [(outputIndex, x)]
