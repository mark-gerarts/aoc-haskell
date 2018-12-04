module Day03a where

import Text.Regex.PCRE
import qualified Data.HashMap.Strict as M

data Claim = Claim
  { position :: Position
  , width :: Int
  , height :: Int
  , id :: Int
  } deriving (Show)

type Position = (Int, Int)

main :: IO ()
main = do
  s <- readFile "input/Day03a.txt"
  print $ solve s

solve :: String -> Int
solve s =
  let claims = map parseClaim $ lines s
      positions = allPositions claims
      positionsWithCounts = applyPositions positions M.empty
      in countOverlapping positionsWithCounts

applyPositions :: [Position] -> M.HashMap Position Int -> M.HashMap Position Int
applyPositions [] m = m
applyPositions (p:ps) m = applyPositions ps (M.insertWith inc p 1 m)
  where
    inc new old = new + old

countOverlapping :: M.HashMap Position Int -> Int
countOverlapping = M.size . M.filter (>1)

parseClaim :: String -> Claim
parseClaim s = Claim (x, y) width height id
  where
    matches = s =~ "\\d+" :: AllTextMatches [] String
    [id, x, y, width, height] = map readInt $ getAllTextMatches matches

readInt :: String -> Int
readInt = read

allPositions :: [Claim] -> [Position]
allPositions = concatMap getPositions

getPositions :: Claim -> [Position]
getPositions (Claim (x, y) w h _) =
  [(x', y') | x' <- [x .. (x + w - 1)], y' <- [y .. (y + h - 1)]]
