module Main where

import Data.List.Split

data Node = Node String [Node] Int deriving (Show)
type Object = String
type Orbit = (Object, Object)

main :: IO ()
main = do
  putStrLn "hello world"

orbits :: Object -> Object -> Orbit
orbits a b = (a, b)

parseSingle :: String -> Orbit
parseSingle str = a `orbits` b
  where
    [a, b] = splitOn ")" str

parseInput :: String -> [Orbit]
parseInput = map parseSingle . lines

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
