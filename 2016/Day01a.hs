module Day01a where

import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import Data.List.Split

type Position = (Int, Int, Orientation)
data Orientation = North | East | South | West deriving (Show, Eq)
data Direction = Left | Right deriving (Show, Eq)
type Instruction = (Direction, Int)

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  print $ solve s

solve :: String -> Int
solve s =
  let instructions = parseInput s
      endPosition = foldr followInstruction (0, 0, North) (reverse instructions)
  in distance (0,0,North) endPosition

turn :: Direction -> Orientation -> Orientation
turn dir o = dirs !! (newIndex `mod` 4)
  where
    dirs = [North, East, South, West]
    currentIndex = fromJust $ elemIndex o dirs
    newIndex = if dir == Left then currentIndex - 1 else currentIndex + 1

step :: Position -> Position
step (x, y, North) = (x, y + 1, North)
step (x, y, East) = (x + 1, y, East)
step (x, y, South) = (x, y - 1, South)
step (x, y, West) = (x - 1, y, West)

followInstruction :: Instruction -> Position -> Position
followInstruction (dir, n) (x, y, o) = applyN n step (x, y, newOrientation)
  where
    newOrientation = turn dir o
    applyN n f = foldr (.) id (replicate n f)

parse :: String -> Instruction
parse s = (dir, n)
  where
    (dirString, numberString) = (head s, tail s)
    dir = if dirString == 'L' then Left else Right
    n = read numberString

parseInput :: String -> [Instruction]
parseInput = map parse . splitOn ", " . init

distance :: Position -> Position -> Int
distance (x1, y1, _) (x2, y2, _) = abs (x2 - x1) + abs (y2 - y1)
