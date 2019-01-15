module Day06a where

import Text.Regex.PCRE
import qualified Data.Map.Strict as M
import Data.List

data Action = TurnOn | TurnOff | Toggle
type Coordinate = (Int, Int)
type Rectangle = (Coordinate, Coordinate)
type Grid = M.Map Coordinate Int
type Instruction = (Action, Rectangle)

main :: IO ()
main = do
  s <- readFile "input/Day06.txt"
  print $ solve s

solve :: String -> Int
solve s = sumMap $ foldl' (flip performInstruction) M.empty instructions
  where
    instructions = map parse $ lines s

parse :: String -> Instruction
parse s = (action, rect)
  where
    numbers = getAllTextMatches (s =~ "\\d+" :: AllTextMatches [] String)
    [x1, y1, x2, y2] = map read numbers
    rect = ((x1, y1), (x2, y2))
    action | "turn off" `isPrefixOf` s = TurnOff
           | "turn on" `isPrefixOf` s = TurnOn
           | "toggle" `isPrefixOf` s = Toggle
           | otherwise = error "No parse"


rectangleToCoordinates :: Rectangle -> [Coordinate]
rectangleToCoordinates ((x1, y1), (x2, y2)) =
  [(x, y) | x <- [x1..x2], y <- [y1..y2]]

performInstruction :: Instruction -> Grid -> Grid
performInstruction (action, rect) g =
  foldl' (\g' c -> doForCoordinate action c g') g (rectangleToCoordinates rect)


incrementValue :: Ord k => k -> M.Map k Int -> M.Map k Int
incrementValue k = M.insertWith (\_ o -> o + 1) k 1

decrementValue :: Ord k => k -> M.Map k Int -> M.Map k Int
decrementValue k = M.insertWith (\_ o -> if o > 0 then o - 1 else 0) k 1

doForCoordinate :: Action -> Coordinate -> Grid -> Grid
doForCoordinate TurnOn c g = incrementValue c g
doForCoordinate TurnOff c g = decrementValue c g
doForCoordinate Toggle c g = incrementValue c (incrementValue c g)

sumMap :: M.Map k Int -> Int
sumMap = sum . M.elems
