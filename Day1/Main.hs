module Day1.Main (
	task1,
	task2
) where

import AOC

task1 = do
	input <- readAsListOfInt "Day1/input.txt"
	print $ calcIncrements 1 input

task2 = do
	input <- readAsListOfInt "Day1/input.txt"
	print $ calcIncrements 3 input

calcIncrements :: Int -> [Int] -> Int
calcIncrements increment input = sum $ zipWith (\x y -> if x < y then 1 else 0) input $ drop increment input