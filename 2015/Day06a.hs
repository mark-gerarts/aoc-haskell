module Day06a where

import Text.Regex.PCRE
import qualified Data.Map.Strict as M

type Coordinate = (Int, Int)
type Rectangle = (Coordinate, Coordinate)
type Grid = M.Map Coordinate Bool

main :: IO ()
main = do
  s <- readFile "input/Day06.txt"
  print $ solve s

solve :: String -> Int
solve s = M.size $ M.filter (==True) $ foldl (\g i -> i g) M.empty instructions
  where
    instructions = map (toggleRectangle . parse) $ lines s

parse :: String -> Rectangle
parse s = ((x1, y1), (x2, y2))
  where
    numbers = getAllTextMatches (s =~ "\\d+" :: AllTextMatches [] String)
    [x1, y1, x2, y2] = map read numbers

rectangleToCoordinates :: Rectangle -> [Coordinate]
rectangleToCoordinates ((x1, y1), (x2, y2)) =
  [(x, y) | x <- [x1..x2], y <- [y1..y2]]

toggleCoordinate :: Coordinate -> Grid -> Grid
toggleCoordinate c = M.insertWith (\_ old -> not old) c True

toggleRectangle :: Rectangle -> Grid -> Grid
toggleRectangle r g = foldl (flip toggleCoordinate) g coords
  where
    coords = rectangleToCoordinates r
