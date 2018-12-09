module Lib.Matrix where

import qualified Data.Vector as V
import Data.Maybe

data Matrix a = Matrix { nrows :: Int
                       , ncols :: Int
                       , contents :: V.Vector a
                       }

instance (Show a) => Show (Matrix a) where
  show m = "M: " ++ show (contents m)

instance Foldable Matrix where
  foldr f z m = foldr f z (contents m)

getRow :: Int -> Matrix a -> V.Vector a
getRow row m = V.fromList $ map (m !) rowPositions
  where
    rowPositions = [(x,row) | x <- [1..(ncols m)]]

getCol :: Int -> Matrix a -> V.Vector a
getCol col m = V.fromList $ map (m !) colPositions
  where
    colPositions = [(col,y) | y <- [1..(nrows m)]]

setElem :: a -> (Int, Int) -> Matrix a -> Matrix a
setElem x pos m = Matrix (nrows m) (ncols m) ((V.//) (contents m) [(i, x)])
  where
    i = pos2index pos m

safeGet :: Int -> Int -> Matrix a -> Maybe a
safeGet x y m | x < minX || x > maxX || y < minY || y > maxY = Nothing
              | otherwise = Just ((!) m (x,y))
  where
    minX = 1
    maxX = ncols m
    minY = 1
    maxY = nrows m

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix n m f = Matrix n m v
  where
    vectorGenerator i = f ((i `mod` m) + 1, (i `div` m) + 1)
    v = V.generate (n*m) vectorGenerator

(!) :: Matrix a -> (Int, Int) -> a
(!) m pos = (V.!) (contents m) (pos2index pos m)

pos2index :: (Int, Int) -> Matrix a -> Int
pos2index (x,y) m = (y -1) * nrows m + (x - 1)
