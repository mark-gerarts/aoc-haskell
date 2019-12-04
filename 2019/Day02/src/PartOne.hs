module Main where

import Data.List.Split
import qualified Data.Vector as V

type Instructions = V.Vector Int

type IntCode = Int

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructionList = parseInstructionList input
      restoredList = (V.//) instructionList [(1, 12), (2, 2)]
      executedList = eval 0 restoredList
      firstElement = (V.!) executedList 0
    in  print firstElement

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
