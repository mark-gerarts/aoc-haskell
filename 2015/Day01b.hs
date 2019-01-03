module Day01b where

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  print $ climbApartment s

climbApartment :: String -> Int
climbApartment input = go input 0 1
  where
    go "" _ _ = error "Never entered the basement"
    go (x:xs) n i
      | n < 0 = i - 1
      | otherwise =
          case x of
            '(' -> go xs (n + 1) (i + 1)
            ')' -> go xs (n - 1) (i + 1)
            _ -> go xs n i
