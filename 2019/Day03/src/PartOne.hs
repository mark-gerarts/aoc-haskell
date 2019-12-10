module Main where

import Prelude hiding (Right, Left)
import Data.List
import Data.List.Split
import qualified Data.Set as S

type Position = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Show)
type Instruction = (Direction, Int)
type Path = [Position]
type Line = (Position, Position)
type Wire = [Line]

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ distanceToNearest (0, 0) $ getIntersections $ parseInput input

-- @todo: rework with segments and their intersections. It isn't feasible
--        to story every single position.

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

-- Parses the input in a list of wires
parseInput :: String -> [Wire]
parseInput = map (pathToWire . walk (0, 0) . parseInstructions) . lines

parseInstructions :: String -> [Instruction]
parseInstructions = map parseInstruction . splitOn ","

-- Constructs a path of points which form the lines of the wire
walk :: Position -> [Instruction] -> Path
walk start = foldl (\path instruction -> path ++ [nextPosition path instruction]) [start]
  where
    nextPosition path = followInstruction (last path)

pathToWire :: Path -> Wire
pathToWire path = zip path (tail path)

followInstruction :: Position -> Instruction -> Position
followInstruction (startX, startY) (direction, amount) =
    case direction of
        Up -> (startX, startY + amount)
        Down -> (startX, startY - amount)
        Right -> (startX + amount, startY)
        Left -> (startX - amount, startX)

getIntersections :: [Wire] -> [Position]
getIntersections paths = undefined

intersects :: Line -> Line -> Bool
intersects ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2)) = regularIntersect
    where
      regularIntersect = ((ax1 <= bx1 && ax2 >= bx1) || (ax2 <= bx1 && ax1 >= bx1))
        && ((by1 <= ay1 && by2 >= ay1) || (by2 <= ay1 && by1 >= ay1))
-- @todo

distanceToNearest :: Position -> [Position] -> Int
distanceToNearest start positions = minimum $ map (distance start) positions

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) +  abs(y1 - y2)

testInput :: String
testInput = "R8,U5,L5,D3\nU7,R6,D4,L4"
