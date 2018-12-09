{-# LANGUAGE FlexibleInstances #-}

module Infi where

import Prelude hiding (Right, Left)
import Data.List
import Data.Char

data Direction = Up | Right | Down | Left deriving (Show, Eq, Ord)
type Room = [Direction]
type Dimensions = (Int, Int)

parseInput :: String -> ([Room], Dimensions)
parseInput s = (map parseTile (concat rows), dimensions)
  where
    rows = lines s
    dimensions = (length rows, length (head rows))

parseTile :: Char -> Room
parseTile '║' = [Up, Down]
parseTile '╔' = [Right, Down]
parseTile '╗' = [Down, Left]
parseTile '╠' = [Up, Right, Down]
parseTile '╦' = [Right, Down, Left]
parseTile '╚' = [Up, Right]
parseTile '╝' = [Up, Left]
parseTile '╬' = [Up, Right, Down, Left]
parseTile '╩' = [Up, Right, Left]
parseTile '═' = [Right, Left]
parseTile '╣' = [Up, Down, Left]
parseTile _ = error "No parse"

sampleInput :: String
sampleInput =
  "╔═╗║\n\
  \╠╗╠║\n\
  \╬╬╣╬\n\
  \╚╩╩═"
