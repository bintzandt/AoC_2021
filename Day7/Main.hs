module Day7.Main (
	task1,
	task2,
) where

import AOC

type Pos = Int
type Max = Pos
type Current = Pos
type FuelRequired = Int
type CrabPositions = [Int]

task1 = do
	crabs <- readStringAsListOfInt "Day7/input.txt"
	let maxPos = maximum crabs
	let minPos = minimum crabs
	print $ calculateMinimalFuel minPos maxPos crabs calculateFuel

task2 = do
	crabs <- readStringAsListOfInt "Day7/input.txt"
	let maxPos = maximum crabs
	let minPos = minimum crabs
	print $ calculateMinimalFuel minPos maxPos crabs calculateFuel'

-- Recursively calculate the minimal amount of fuel required to move all crabs to the same position.
calculateMinimalFuel :: Current -> Max -> CrabPositions -> (Pos -> CrabPositions -> FuelRequired) -> FuelRequired
calculateMinimalFuel current maxi positions f
	| current == maxi = f current positions
	| otherwise = if fuelForThisPos < fuelForOtherPos then fuelForThisPos else fuelForOtherPos
	where
		fuelForThisPos = f current positions
		fuelForOtherPos = calculateMinimalFuel (current+1) maxi positions f

-- Fuel calculations for Part 1.
-- The fuel required is simply the distance between the two positions.
calculateFuel :: Pos -> CrabPositions -> FuelRequired
calculateFuel goto pos = sum $ map (\x -> abs (x-goto)) pos

-- Alternate sum function.
-- Calculates the sum of all integers smaller than the input.
sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n-1)

-- Due to Haskells Lazyness, we can create an infinite list of sums.
-- This ensures that sums are cached and are calculated once at most.
sums = map sum' [0..]

-- Fuel calculations for Part 2.
calculateFuel' :: Pos -> CrabPositions -> FuelRequired
calculateFuel' goto pos = sum $ map (\x -> sums !! abs (x-goto)) pos