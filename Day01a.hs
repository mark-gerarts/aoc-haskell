module Day01a where

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  print . sum . map string2int . lines $ s

string2int :: String -> Int
string2int x | head x == '+' = read $ tail x
             | otherwise = read x
