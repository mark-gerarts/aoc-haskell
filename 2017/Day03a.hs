module Day03a where

import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe

data Direction = Up | Right | Down | Left deriving (Show)
type Position = (Int, Int)

solve :: Int -> Int
solve x = manhattanDistance pos (0,0)
  where
    (_, pos) = fromJust $ find ((==x) . fst) spiral

move :: Direction -> Position -> Position
move Up (x, y) = (x, y - 1)
move Right (x, y) = (x + 1, y)
move Down (x, y) = (x, y + 1)
move Left (x, y) = (x - 1, y)

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = abs x1 - x2 + abs y1 - y2

-- Builds a spiral: a list of indices with their respective positions on the
-- grid.
spiral :: [(Int, Position)]
spiral = go (1,(0,0)) spiralDirections
  where
    go (x,pos) (dir:dirs) = (x,pos) : go (x + 1, move dir pos) dirs

-- Builds a list of directions that should be followed from the center of the
-- spiral to build the spiral
spiralDirections :: [Direction]
spiralDirections = concatMap (\(dir,i) -> replicate i dir) (zip directions spacings)

directions :: [Direction]
directions = cycle [Right, Up, Left, Down]

spacings :: [Int]
spacings = go 1 []
  where
    go i is = i : i : go (i+1) is
