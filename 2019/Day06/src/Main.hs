module Main where

import Data.List
import Data.List.Split
import qualified Data.Map as M

data Node = Node String [Node] Int deriving (Show)
data Object = COM | Object String deriving (Show)
type Orbit = (Object, Object)
type OrbitMap = M.Map Object Object

instance Eq Object where
  COM == COM = True
  Object _ == COM = False
  COM == Object _ = False
  Object a == Object b = a == b

instance Ord Object where
  COM `compare` Object _ = LT
  Object a `compare` Object b = a `compare` b

main :: IO ()
main = do
  input <- readFile "input.txt"
  let orbitMap = parseInput input
  print $ sumAllOrbits orbitMap

mainPartTwo :: IO ()
mainPartTwo = do
  input <- readFile "input.txt"
  let orbitMap = parseInput input
  print $ getDistanceToSanta orbitMap

-- We calculate the distance between santa and me by collecting all the objects
-- both santa and I orbit (the "traces"). Once we have that, we remove the
-- common orbits. What's left is the route I have to travel to santa.
getDistanceToSanta :: OrbitMap -> Int
getDistanceToSanta orbitMap = length $ (myTrace \\ santaTrace) ++ (santaTrace \\ myTrace)
  where
    santaTrace = getOrbitTrace (Object "SAN") orbitMap
    myTrace = getOrbitTrace (Object "YOU") orbitMap

getOrbitTrace :: Object -> OrbitMap -> [Object]
getOrbitTrace COM _ = []
getOrbitTrace object orbitMap = next : getOrbitTrace next orbitMap
  where
    next = orbitMap M.! object

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
