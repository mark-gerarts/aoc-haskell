module Main where

import Prelude hiding (Right, Left)
import Data.List
import Data.List.Split
import qualified Data.Set as S

type Position = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Show)
type Instruction = (Direction, Int)
type Path = [Position]

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ distanceToNearest (0, 0) $ getIntersections $ parseInput input

parseDirection :: Char -> Direction
parseDirection input = case input of
  'U' -> Up
  'R' -> Right
  'D' -> Down
  'L' -> Left
  _ -> error "Invalid direction"

parseInstruction :: String -> Instruction
parseInstruction input = (direction, amount)
  where
    direction = parseDirection $ head input
    amount = read $ tail input

-- Parses the input in a list of paths representing the strings
parseInput :: String -> [Path]
parseInput input = foldl (\c p -> c ++ [walk (0, 0) p]) [firstPath] $ tail instructions
  where
    instructions = map (map parseInstruction) $ map (splitOn ",") $ lines input
    firstPath = walk (0,0) $ head instructions

walk :: Position -> [Instruction] -> Path
walk start = nub . foldl (\c x -> c ++ walkSingle (last c) x) [start]

-- "Walks" according to the given input, gathering all positions seen inbetween.
walkSingle :: Position -> Instruction -> Path
walkSingle (currentX, currentY) (direction, amount) =
  case direction of
    Up -> [(currentX, y) | y <- [currentY..(currentY + amount)]]
    Down -> [(currentX, y) | y <- enumFromThenTo currentY (currentY - 1) (currentY - amount)]
    Right -> [(x, currentY) | x <- [currentX..(currentX + amount)]]
    Left -> [(x, currentY) | x <- enumFromThenTo currentX (currentX - 1) (currentX- amount)]

getIntersections :: [Path] -> [Position]
getIntersections paths = S.toList $ S.delete (0, 0) intersections
  where
    sets = map S.fromList paths
    intersections = foldl1 S.intersection sets

distanceToNearest :: Position -> [Position] -> Int
distanceToNearest start positions = minimum $ map (\p -> distance start p) positions

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) +  abs(y1 - y2)

testInput :: String
testInput = "R8,U5,L5,D3\nU7,R6,D4,L4"
