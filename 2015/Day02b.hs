module Day02b where

import Text.Regex.PCRE
import Data.List

type Dimensions = (Int, Int, Int)

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ solve s

solve :: String -> Int
solve = sum . map (getRibbonLength . parseLine) . lines

parseLine :: String -> Dimensions
parseLine line = (l, h, w)
  where
    dimensions = getAllTextMatches (line =~ "\\d+" :: AllTextMatches [] String)
    [l,w,h] = map read dimensions

getWrappingPaperAmount :: Dimensions -> Int
getWrappingPaperAmount (l, w, h) =  totalSurface + slack
  where
    surfaces = [l*w, w*h, h*l]
    totalSurface = sum surfaces * 2
    slack = minimum surfaces

getRibbonLength :: Dimensions -> Int
getRibbonLength (l, w, h) = smallestPerimeter + bowLength
  where
    smallestPerimeter = (*2) $ sum $ take 2 $ sort [l,w,h]
    bowLength = l*w*h
