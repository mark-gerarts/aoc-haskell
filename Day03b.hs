module Day03b where

import Text.Regex.PCRE
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

data Claim = Claim
  { position :: Position
  , width :: Int
  , height :: Int
  , id :: Int
  } deriving (Show)

-- An x-y coordinate
type Position = (Int, Int)
-- A position with an ID
type MarkedPosition = (Int, Position)
type PositionMap = M.HashMap Position (S.Set Int)
-- Maps ID to a boolean indicating if the pattern overlaps
type OverlapMap = M.HashMap Int Bool

main :: IO ()
main = do
  s <- readFile "input/Day03.txt"
  print $ solve s

solve :: String -> Int
solve s =
  let claims = map parseClaim $ lines s
      positions = allPositions claims
      positionsWithCounts = applyPositions positions M.empty M.empty
      in findNonOverlappingId positionsWithCounts

findNonOverlappingId :: OverlapMap -> Int
findNonOverlappingId = head . M.keys . M.filter (==False)

-- Verryy ugly code. Basically we keep a map of positions and the claim IDs. If
-- a position overlaps we add it to the OverlapMap. The result should be a map
-- where only one id has a false value.
applyPositions :: [MarkedPosition] -> PositionMap -> OverlapMap -> OverlapMap
applyPositions [] pm seen = seen
applyPositions ((id,pos):ps) pm seen =
  case M.lookup pos pm of
    Just ids -> applyPositions ps (M.insert pos (S.insert id ids) pm) (addIdsToSeen (S.insert id ids))
    Nothing -> case M.lookup id seen of
      Just x -> applyPositions ps (M.insert pos (S.singleton id) pm) seen
      Nothing -> applyPositions ps (M.insert pos (S.singleton id) pm) (M.insert id False seen)
  where
    addIdsToSeen = S.foldl (\s id -> M.insert id True s) seen

countOverlapping :: M.HashMap Position Int -> Int
countOverlapping = M.size . M.filter (>1)

parseClaim :: String -> Claim
parseClaim s = Claim (x, y) width height id
  where
    matches = s =~ "\\d+" :: AllTextMatches [] String
    [id, x, y, width, height] = map readInt $ getAllTextMatches matches

readInt :: String -> Int
readInt = read

allPositions :: [Claim] -> [MarkedPosition]
allPositions = concatMap getPositions

getPositions :: Claim -> [MarkedPosition]
getPositions (Claim (x, y) w h id) =
  [(id, (x', y')) | x' <- [x .. (x + w - 1)], y' <- [y .. (y + h - 1)]]
