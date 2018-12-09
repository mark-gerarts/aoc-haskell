module Day07a where

import Text.Regex.PCRE
import Data.Char
import Data.List

type Node = Char
type Graph = [(Node, [Node], [Node])]

assignNexts :: Graph -> Graph
assignNexts = M.foldl

toNodes :: [(Char, Char)] -> Graph
toNodes list = go list M.empty
  where
    go [] g = g
    go ((id,prev):xs) g = case findNode g of
      Just (id, prevs, nexts) ->

findNode :: Char -> Graph -> Maybe (Node, [Node], [Node])
findNode c = find (\(c', _, _) -> c' == c)

parseInput :: String -> [(Node, Node)]
parseInput = map parseLine . lines

parseLine :: String -> (Node, Node)
parseLine s = (id, prev)
  where
    id = head (s =~ "(?<=step )[A-Z]{1}" :: String)
    prev = head (s =~ "(?<=Step )[A-Z]{1}" :: String)

sampleInput :: String
sampleInput =
  "Step C must be finished before step A can begin.\n\
  \Step C must be finished before step F can begin.\n\
  \Step A must be finished before step B can begin.\n\
  \Step A must be finished before step D can begin.\n\
  \Step B must be finished before step E can begin.\n\
  \Step D must be finished before step E can begin.\n\
  \Step F must be finished before step E can begin."
