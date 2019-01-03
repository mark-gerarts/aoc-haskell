module Day01a where

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  print $ climbApartment s

climbApartment :: String -> Int
climbApartment input = go input 0
  where
    go "" n = n
    go (x:xs) n =
      case x of
        '(' -> go xs (n + 1)
        ')' -> go xs (n - 1)
        _ -> go xs n
