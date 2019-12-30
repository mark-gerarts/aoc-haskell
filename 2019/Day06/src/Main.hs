module Main where

import Data.List.Split
import qualified Data.Map as M

data Node = Node String [Node] Int deriving (Show)
data Object = COM | Object String deriving (Show)
type Orbit = (Object, Object)
type OrbitMap = M.Map Object Object

instance Eq Object where
  COM == COM = True
  Object _ == COM = False
  Object a == Object b = a == b

instance Ord Object where
  COM `compare` Object _ = LT
  Object a `compare` Object b = a `compare` b

main :: IO ()
main = do
  input <- readFile "input.txt"
  let orbitMap = parseInput input
  print $ sumAllOrbits orbitMap

orbits :: Object -> Object -> Orbit
orbits a b = (a, b)

sumAllOrbits :: OrbitMap -> Int
sumAllOrbits orbitMap = sum $ map (`getAllOrbits` orbitMap) $ M.keys orbitMap

getAllOrbits :: Object -> OrbitMap -> Int
getAllOrbits object orbitMap = indirectOrbits + directOrbits
  where
    indirectOrbits = getIndirectOrbits object orbitMap
    directOrbits = getDirectOrbits object orbitMap

getIndirectOrbits :: Object -> OrbitMap -> Int
getIndirectOrbits COM _ = 0
getIndirectOrbits object orbitMap = go object - 1
  where
    go COM = 0
    go obj = 1 + go (orbitMap M.! obj)

getDirectOrbits :: Object -> OrbitMap -> Int
getDirectOrbits COM _ = 0
getDirectOrbits _ _ = 1

parseSingle :: String -> Orbit
parseSingle str = parse a `orbits` parse b
  where
    [a, b] = splitOn ")" str
    parse "COM" = COM
    parse name = Object name

parseInput :: String -> OrbitMap
parseInput input = foldl (\map (a,b) -> M.insert b a map) M.empty orbits
  where
    orbits = (map parseSingle . lines) input

testInput :: String
testInput = "COM)B\n\
\B)C\n\
\C)D\n\
\D)E\n\
\E)F\n\
\B)G\n\
\G)H\n\
\D)I\n\
\E)J\n\
\J)K\n\
\K)L"
