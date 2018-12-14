module Day05b where

import qualified Data.Map as M

data InstructionSet = InstructionSet
  { instructions :: M.Map Int Int
  , currentIndex :: Int
  , jumpCount :: Int
  } deriving (Show)

main :: IO ()
main = do
  s <- readFile "input/Day05.txt"
  print $ solve s

solve :: String -> Int
solve s = escape is
  where
    is = InstructionSet (M.fromList (zip [0..] (parseInput s))) 0 0

parseInput :: String -> [Int]
parseInput = map read . lines

escape :: InstructionSet -> Int
escape (InstructionSet xs i n) =
  case instruction of
    Nothing -> n
    Just _ -> escape $! jump (InstructionSet xs i n)
  where
    instruction = xs M.!? i

jump :: InstructionSet -> InstructionSet
jump (InstructionSet xs i n) = InstructionSet newInstructions newIndex newN
  where
    currentInstruction = xs M.! i
    delta | currentInstruction >= 3 = -1
          | otherwise = 1
    newInstructions = M.insert i (currentInstruction + delta) xs
    newIndex = i + currentInstruction
    newN = n + 1
