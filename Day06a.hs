module Day06a where

import Text.Regex.PCRE
import qualified Data.Map.Strict as M
import Data.List
import qualified Data.Vector as V
import Data.Ord
import Data.Maybe
-- I rolled my own matrix library in a futile attempt to get better performance
import Lib.Matrix

type Pos = (Int, Int)
data Cell = Empty | ClosestTo Int | Value Int deriving (Eq)
type Grid = Matrix Cell

instance Show Cell where
  show Empty = "."
  show (ClosestTo x) = show x
  show (Value x) = "[" ++ show x ++ "]"

main :: IO ()
main = do
  s <- readFile "input/Day06.txt"
  print $ solve s

solve :: String -> Int
solve input =
  let grid = transformGrid $ parseInput input
      interiors = onlyInteriors grid (getCounts grid)
      (_,count) = maximumBy (comparing snd) (M.toList interiors)
  in count + 1


onlyInteriors :: Grid -> M.Map Int Int -> M.Map Int Int
onlyInteriors grid counts = M.filterWithKey (\k _ -> k `notElem` exteriorIds) counts
  where
    exteriorIds = getExteriorIds grid

getCounts :: Grid -> M.Map Int Int
getCounts = foldl updateCount M.empty
  where
    updateCount m c = case c of
      Empty -> m
      Value _ -> m
      ClosestTo id -> M.insertWith (+) id 1 m

getExteriorIds :: Grid -> [Int]
getExteriorIds grid = [x | ClosestTo x <- exteriorVals]
  where
    top = getRow 1 grid
    right = getCol (ncols grid) grid
    bottom = getRow (nrows grid) grid
    left = getCol 1 grid
    exteriorVals = nub $ V.toList $ V.concat [top, right, bottom, left]

transformGrid :: Grid -> Grid
transformGrid grid = foldl (\g' p -> setElem (transformCell p g') p g') grid ps
  where
    ps = allPositions (nrows grid) (ncols grid)

allPositions :: Int -> Int -> [Pos]
allPositions n m = [(x,y) | x <- [1..n], y <- [1..m]]

transformCell :: Pos -> Grid -> Cell
transformCell pos grid = case grid ! pos of
  Empty -> if length vals > 1 then Empty
           else ClosestTo (head vals)
                where vals = getClosestValues pos grid
  Value x -> Value x

getClosestValues :: Pos -> Grid -> [Int]
getClosestValues pos grid = go 1
  where
    go :: Int -> [Int]
    go n | null values = go (n + 1)
         | otherwise = values
      where
        values = getValuesAtPositions (getSurroundingPositions pos n) grid

getValuesAtPositions :: [Pos] -> Grid -> [Int]
getValuesAtPositions ps grid =
  [fromJust val | (i,j) <- ps
              , let x = safeGet i j grid
              , isJust x
              , let val = getValue (fromJust x)
              , isJust val]

getValue :: Cell -> Maybe Int
getValue (Value x) = Just x
getValue _ = Nothing

getSurroundingPositions :: Pos -> Int -> [Pos]
getSurroundingPositions (x,y) n = nub positions
  where
    minX = x - n
    maxX = x + n
    minY = y - n
    maxY = y + n
    positions = [(x', y') | x' <- [minX..maxX], y' <- (getYsForX x')]
    getYsForX x' = [(y + delta x'), (y - delta x')]
      where
        delta x' | (x' - minX) < ((maxX - minX) `div` 2) = x' - minX
                 | otherwise = abs (x' - maxX)


parseInput :: String -> Grid
parseInput s = matrix n m getValueForPosition
  where
    coordinates = map parseLine $ lines s
    (n, m) = dimensions coordinates
    namedCoordinates = M.fromList $ zip coordinates [1..]
    getValueForPosition pos =
      case M.lookup pos namedCoordinates of
       Just id -> Value id
       Nothing -> Empty

dimensions :: [Pos] -> (Int, Int)
dimensions ps = (maximum xs, maximum ys)
  where
    xs = map fst ps
    ys = map snd ps

parseLine :: String -> Pos
parseLine s =
  case getAllTextMatches (s =~ "\\d+" :: AllTextMatches [] String) of
    [x, y] -> (read x + 1, read y + 1)
    _ -> error "Unable to parse row"
