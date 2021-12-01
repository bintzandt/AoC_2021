module Day1.Main (
	task1,
	task2
) where

import AOC

task1 = do
	input <- readAsListOfInt "Day1/input.txt"
	print $ calcIncrements input

task2 = do
	input <- readAsListOfInt "Day1/input.txt"
	print $ calcIncrements $ map sum $ partition input []

calcIncrements :: [Int] -> Int
calcIncrements input = snd $ foldl (\(prev, count) curr -> if curr > prev then (curr, count+1) else (curr, count)) (head input, 0) (tail input)

partition :: [Int] -> [[Int]] -> [[Int]]
partition [] result = result
partition (x:[]) result = result
partition (x:y:[]) result = result
partition input result = partition (drop 1 input) (result ++ [(take 3 input)])