module Main where

import Prelude hiding (Right, Left)
import Data.List
import Data.List.Split
import qualified Data.Set as S

type Position = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Show)
type Instruction = (Direction, Int)
type Wire = [Position]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let wires = parseInput input
      w1 = head wires
      w2 = last wires
      intersections = getIntersections w1 w2
  print $ distanceToNearest (0, 0) intersections

parseDirection :: Char -> Direction
parseDirection input = case input of
  'U' -> Up
  'R' -> Right
  'D' -> Down
  'L' -> Left
  _ -> error "Invalid direction"

-- "Walks" according to the instructions, collecting the passed points and thus
-- forming the wire.
walk :: Position -> [Instruction] -> [Position]
walk s is = nub $ foldl (\c i -> c ++ lineTo (last c) i) [(0, 0)] is
  where
    lineTo (sx, sy) i = case i of
      (Up, n) -> [(sx, y) | y <- [sy..(sy + n)]]
      (Down, n) -> [(sx, y) | y <- [sy,(sy - 1)..(sy - n)]]
      (Right, n) -> [(x, sy) | x <- [sx..(sx + n)]]
      (Left, n) -> [(x, sy) | x <- [sx,(sx - 1)..(sx - n)]]

parseInput :: String -> [Wire]
parseInput = map parseWire . lines

parseWire :: String -> Wire
parseWire = walk (0,0) . map parseInstruction . splitOn ","

parseInstruction :: String -> Instruction
parseInstruction input = (direction, amount)
  where
    direction = parseDirection $ head input
    amount = read $ tail input

getIntersections :: Wire -> Wire -> [Position]
getIntersections w1 w2 = S.toList $ S.delete (0, 0) intersections
  where
    intersections = S.intersection (S.fromList w1) (S.fromList w2)

distanceToNearest :: Position -> [Position] -> Int
distanceToNearest start positions = minimum $ map (distance start) positions

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) +  abs (y1 - y2)
