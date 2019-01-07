module Day03a where

import Prelude hiding (Left, Right)
import qualified Data.Map.Strict as M

data Direction = Up | Right | Down | Left deriving (Show)
type Position = (Int,Int)
type PositionCount = M.Map Position Int

main :: IO ()
main = do
  s <- readFile "input/Day03.txt"
  print $ solve (init s)

solve :: String -> Int
solve input = M.size posCount
  where
    dirs = map parse input
    startPos = (0,0)
    posCount = followDirections dirs (inc M.empty startPos) startPos
    multiples = M.filter (>1) posCount

move :: Direction -> Position -> Position
move Up (x, y) = (x, y + 1)
move Right (x, y) = (x + 1, y)
move Down (x, y) = (x, y - 1)
move Left (x, y) = (x - 1, y)

parse :: Char -> Direction
parse '^' = Up
parse '>' = Right
parse 'v' = Down
parse '<' = Left
parse _ = error "No parse"

inc :: Ord a => M.Map a Int -> a -> M.Map a Int
inc m x = M.insertWith (+) x 1 m

followDirections :: [Direction] -> PositionCount -> Position -> PositionCount
followDirections [] m _ = m
followDirections (dir:dirs) m p =
  followDirections dirs (inc m newPosition) newPosition
  where
    newPosition = move dir p
