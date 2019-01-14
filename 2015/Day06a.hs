module Day06a where

import Text.Regex.PCRE
import qualified Data.Set as S
import Data.List

data Action = TurnOn | TurnOff | Toggle
type Coordinate = (Int, Int)
type Rectangle = (Coordinate, Coordinate)
type Grid = S.Set Coordinate
type Instruction = (Action, Rectangle)

main :: IO ()
main = do
  s <- readFile "input/Day06.txt"
  print $ solve s

solve :: String -> Int
solve s = S.size $ foldl' (flip performInstruction) S.empty instructions
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
  foldl (\g' c -> doForCoordinate action c g') g (rectangleToCoordinates rect)


doForCoordinate :: Action -> Coordinate -> Grid -> Grid
doForCoordinate TurnOn c g = S.insert c g
doForCoordinate TurnOff c g = S.delete c g
doForCoordinate Toggle c g | S.member c g = S.delete c g
                           | otherwise = S.insert c g
