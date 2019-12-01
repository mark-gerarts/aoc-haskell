module Day01a where

main :: IO()
main = do
    s <- readFile "input.txt"
    print . sum . map (calculateFuelConsumption . read) . lines $ s

fuelForMass :: Int -> Int
fuelForMass m = m `div` 3 - 2

calculateFuelConsumption :: Int -> Int
calculateFuelConsumption mass = go 0 mass
    where
        go total m
            | fuelForMass m <= 0 = total
            | otherwise = go (total + fuelForMass m) (fuelForMass m)
