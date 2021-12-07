module Day6.Main (
	task1,
	task2,
) where

import Data.List.Split
import AOC

task1 = do
	input <- readFile "Day6/input.txt"
	let fish = map (\x -> read x :: Int) $ splitOn "," input
	print $ length $ simulateDays 80 fish

simulateDays :: Int -> [Int] -> [Int]
simulateDays 0 fish = fish
simulateDays d fish = simulateDays (d-1) simulation
	where
		simulation = simulateFishDay fish

simulateFishDay :: [Int] -> [Int]
simulateFishDay [] = []
simulateFishDay (x:xs)
	-- New fish is created when internal timer is 0
	-- A new fish is created with timer 8
	| x == 0 = [8,6] ++ simulateFishDay xs
	| otherwise = (x-1:simulateFishDay xs)

task2 = do
	input <- readFile "Day6/input.txt"
	let fish = map (\x -> read x :: Int) $ splitOn "," input
	print $ sum $ simulateFishDay' 256 $ countFishPerDay fish

countFishPerDay :: [Int] -> [Int]
countFishPerDay fish = do
	age <- [0..8]
	return $ length $ filter (==age) fish

simulateFishDay' :: Int -> [Int] -> [Int]
simulateFishDay' 0 list = list
simulateFishDay' n (zero:one:two:three:four:five:six:seven:eight:[]) = simulateFishDay' (n-1) (one:two:three:four:five:six:(seven+zero):eight:[zero])
