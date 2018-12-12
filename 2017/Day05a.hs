module Day05a where

import qualified Data.Vector as V

data InstructionSet = InstructionSet
  { instructions :: V.Vector Int
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
    is = InstructionSet (V.fromList (parseInput s)) 0 0

parseInput :: String -> [Int]
parseInput = map read . lines

update :: V.Vector a -> Int -> a -> V.Vector a
update v i x = v V.// [(i, x)]

escape :: InstructionSet -> Int
escape (InstructionSet xs i n) =
  case xs V.!? i of
    Just _ -> escape $ jump (InstructionSet xs i n)
    Nothing -> n

jump :: InstructionSet -> InstructionSet
jump (InstructionSet xs i n) = InstructionSet newInstructions newIndex (n + 1)
  where
    currentInstruction = xs V.! i
    newInstructions = update xs i (currentInstruction + 1)
    newIndex = i + currentInstruction
