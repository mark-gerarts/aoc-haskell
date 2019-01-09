module Day05a where

main :: IO ()
main = do
  s <- readFile "input/Day05.txt"
  print $ length $ filter isValid $ lines s

isVowel :: Char -> Bool
isVowel c = c `elem` "aeoui"

contains3Vowels :: String -> Bool
contains3Vowels = (>2) . length . filter isVowel

contains2consecutive :: String -> Bool
contains2consecutive (a:b:cs) | a == b = True
                              | otherwise = contains2consecutive (b:cs)
contains2consecutive _ = False

containsBlacklisted :: String -> Bool
containsBlacklisted (a:b:cs)
  | [a, b] `elem` blacklist = True
  | otherwise = containsBlacklisted (b:cs)
  where
    blacklist = ["ab", "cd", "pq", "xy"]
containsBlacklisted _ = False

isValid :: String -> Bool
isValid s = all (==True) $ map ($s) rules
  where
    rules = [contains3Vowels, contains2consecutive, not . containsBlacklisted]
