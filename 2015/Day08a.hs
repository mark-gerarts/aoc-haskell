module Day08a where

main :: IO ()
main = do
  s <- readFile "input/Day08.txt"
  print $ solve s

solve :: String -> Int
solve = sum . map (uncurry (-) . lengths) . lines

lengths :: String -> (Int, Int)
lengths s = go s 0 0
  where
    go :: String -> Int -> Int -> (Int, Int)
    go "" ncode nmem = (ncode, nmem - 2)
    go [_] ncode nmem = go "" (ncode + 1) (nmem + 1)
    go (a:b:cs) ncode nmem
      | a /= '\\' = go (b:cs) (ncode + 1) (nmem + 1)
      | b == 'x' = go (drop 2 cs) (ncode + 4) (nmem + 1)
      | b == '\\' || b == '"' = go cs (ncode + 2) (nmem + 1)
      | otherwise = error "Unsopprted escape sequence"
