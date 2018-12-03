module Day03a where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import Text.Regex.PCRE

data Claim = Claim
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , id :: Int
  } deriving (Show)

type Fabric = V.Vector (V.Vector Int)

main :: IO ()
main = do
  s <- readFile "input/Day03a.txt"
  let claims = map parseClaim $ lines s
      fabric = initFabric 2000
      appliedFabric = applyClaims fabric claims
   in print $ countDoubleSquares appliedFabric

parseClaim :: String -> Claim
parseClaim s = Claim x y width height id
  where
    matches = s =~ "\\d+" :: AllTextMatches [] String
    [id, x, y, width, height] = map readInt $ getAllTextMatches matches

readInt :: String -> Int
readInt = read

sampleClaim :: String
sampleClaim = "#123 @ 3,2: 5x4"

initFabric :: Int -> Fabric
initFabric n = V.fromList $ replicate n $ V.fromList $ replicate n 0

incrementFabricAt :: Fabric -> Int -> Int -> Fabric
incrementFabricAt f x y = (V.//) f [(y, updatedRow)]
  where
    row = f V.! y
    currentVal = row V.! x
    updatedRow = row V.// [(x, currentVal + 1)]

applyClaims :: Fabric -> [Claim] -> Fabric
applyClaims = foldl applyClaim

applyClaim :: Fabric -> Claim -> Fabric
applyClaim f c = foldl (\f' (x,y) -> incrementFabricAt f' x y) f positions
  where
    positions = getPositions c

getPositions :: Claim -> [(Int, Int)]
getPositions (Claim x y w h _) =
  [(x', y') | x' <- [x .. (x + w)], y' <- [y .. (y + h)]]

countDoubleSquares :: Fabric -> Int
countDoubleSquares = V.sum . V.map foldRow
  where
    foldRow = V.length . V.filter (>1)
