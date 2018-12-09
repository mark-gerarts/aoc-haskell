module Day03b where

import Prelude hiding (Left, Right)
import Data.Maybe
import qualified Data.Map.Strict as M

data Direction = Up | Right | Down | Left deriving (Show)
type Position = (Int, Int)
type Grid = M.Map Position Int

solve :: Int -> Int
solve = spiral

neighbourSum :: Position -> Grid -> Int
neighbourSum pos grid = sum neighbourVals
  where
    neighbourVals = mapMaybe (\p -> grid M.!? p) (neighbours pos)

neighbours :: Position -> [Position]
neighbours (x, y) = [(x', y') | x' <- [x - 1, x, x + 1]
                              , y' <- [y - 1, y, y + 1]
                              , not (x == x' && y == y')]

move :: Direction -> Position -> Position
move Up (x, y) = (x, y - 1)
move Right (x, y) = (x + 1, y)
move Down (x, y) = (x, y + 1)
move Left (x, y) = (x - 1, y)

spiral :: Int -> Int
spiral max = go M.empty (0,0) spiralDirections
  where
    go grid pos (dir:dirs) = if newValue > max then newValue
                             else go newGrid (move dir pos) (dirs)
      where
        newValue = neighbourSum pos grid
        newGrid = M.insert pos (if newValue == 0 then 1 else newValue) grid

spiralDirections :: [Direction]
spiralDirections = concatMap (\(dir,i) -> replicate i dir) (zip directions spacings)

directions :: [Direction]
directions = cycle [Right, Up, Left, Down]

spacings :: [Int]
spacings = go 1 []
  where
    go i is = i : i : go (i+1) is
