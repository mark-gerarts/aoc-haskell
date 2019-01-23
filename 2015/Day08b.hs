module Day08b where

main :: IO ()
main = do
  s <- readFile "input/Day08.txt"
  print $ solve s

solve :: String -> Int
solve = sum . map (abs . uncurry (-) . lengths) . lines

lengths :: String -> (Int, Int)
lengths s = go s 0 0
  where
    go :: String -> Int -> Int -> (Int, Int)
    go "" ncode nenc = (ncode, nenc + 2)
    go [_] ncode nenc = go [] (ncode + 1) (nenc + 2)
    go (a:b:cs) ncode nenc
      | a == '"' = go (b:cs) (ncode + 1) (nenc + 2)
      | a == '\\' && b == '"' = go cs (ncode + 2) (nenc + 4)
      | a == '\\' && b == 'x' = go cs (ncode + 2) (nenc + 3)
      | otherwise = go (b:cs) (ncode + 1) (nenc + 1)
