module Day3.Main (
	task1,
	task2,
) where

import AOC
import Data.List

task1 = do
	input <- readAsListOfString "Day3/input.txt"
	let lengthOfInput = length input
	let (gamma, epsilon) = calculateRates lengthOfInput $ map sum $ map convertStringToListOfInt $ transpose input
	print (gamma * epsilon)

calculateRates :: Int -> [Int] -> (Int, Int)
calculateRates size list = (binToDec $ map (\x -> if 2 * x > size then 1 else 0) list, binToDec $ map (\x -> if 2 * x > size then 0 else 1) list)

binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l

oxygenGeneratorRating :: [String] -> Int -> String
oxygenGeneratorRating (x:[]) _ = x
oxygenGeneratorRating xs i = oxygenGeneratorRating (filter (\x -> (x !! i) == keep) xs) (i+1)
	where
		transposed = transpose xs
		size = length xs
		keep = if (2 * ((map sum $ map convertStringToListOfInt $ transposed) !! i)) >= size then '1' else '0'

cO2ScrubberRating :: [String] -> Int -> String
cO2ScrubberRating (x:[]) _ = x
cO2ScrubberRating xs i = cO2ScrubberRating (filter (\x -> (x !! i) == keep) xs) (i+1)
	where
		transposed = transpose xs
		size = length xs
		keep = if (2 * ((map sum $ map convertStringToListOfInt $ transposed) !! i)) < size then '1' else '0'

task2 = do
	input <- readAsListOfString "Day3/input.txt"
	let oxygen = oxygenGeneratorRating input 0
	let co2 = cO2ScrubberRating input 0
	print oxygen
	print co2