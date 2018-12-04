module Day01b where

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  let integerList = map string2int . lines $ s
    in print $ findDuplicate (cycle integerList) [0]

string2int :: String -> Int
string2int x | head x == '+' = read $ tail x
             | otherwise = read x

findDuplicate :: [Int] -> [Int] -> Int
findDuplicate [] [x] = x
findDuplicate [] _ = error "No duplicate found"
findDuplicate deltas frequencies
  | newFrequency `elem` frequencies = newFrequency
  | otherwise = findDuplicate (tail deltas) (frequencies ++ [newFrequency])
  where
    newFrequency = head deltas + last frequencies
