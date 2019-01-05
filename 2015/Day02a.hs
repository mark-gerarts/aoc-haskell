module Day02a where

import Text.Regex.PCRE

type Dimensions = (Int, Int, Int)

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ solve s

solve :: String -> Int
solve = sum . map (getWrappingPaperAmount . parseLine) . lines

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
