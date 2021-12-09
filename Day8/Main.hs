module Day8.Main (
	task1,
	task2,
) where

import AOC
import Data.List.Split (splitOn)
import Data.List (sort, (\\), sortOn, foldl')
import Debug.Trace (trace)

import qualified Data.Map as M

task1 = do
	input <- readAsListOfString "Day8/input.txt"
	print $ sum $ map ( isUniqueSegment . length ) $ concat $ map (words . last . splitOn "|") input

isUniqueSegment :: Int -> Int
isUniqueSegment x
	-- 1 = 2 segments
	| x == 2 = 1
	-- 4 = 4 segments
	| x == 4 = 1
	-- 7 = 3 segments
	| x == 3 = 1
	-- 8 = 7 segments
	| x == 7 = 1
	| otherwise = 0

task2 = do
	input <- readAsListOfString "Day8/input.txt"
	let wires = map (map words . splitOn "|") input
	print $ sum . map solve $ wires
	-- print $ map solve wires
	-- print $ map (map sort) $ map last wires


-- solve :: [[String]] -> Int
solve wire =
	let [input, output] = map (map sort) wire;
		mapping = makeMap input
	in foldl' (\result k -> result * 10 + k) 0 . map (lookupInMap mapping) $ output
	where
		lookupInMap mapping x = trace ("Looking up '" ++ x ++ "' in Map: " ++ show mapping ++ "\n") ((M.!) mapping x);

makeMap :: [String] -> M.Map String Int
makeMap xs = 
	let 
		-- Easy digits
		n1 = head . filterLength 2 $ xs
		n4 = head . filterLength 4 $ xs
		n7 = head . filterLength 3 $ xs
		n8 = head . filterLength 7 $ xs
		
		-- Filter all strings with length 5
		length5 = filterLength 5 xs

		-- 3 can be found by looking for the string with length 3 after removing 1
		n3 = head . filter (\s -> length (foldl1 (\\) [s, n1]) == 3) $ length5

		-- 5 and 2 can be found by looking for the string with length 2 respectively 3 after excluding n3 and removing n4
		n5 = head . filter (\s -> length (s \\ n4) == 2) $ (length5 \\ [n3])
		n2 = head . filter (\s -> length (s \\ n4) == 3) $ (length5 \\ [n3])

		-- Filter all strings with length 6
		length6 = filterLength 6 xs

		-- 9 can be found by looking for the string with length 1 after removing 7 and 4
		n9 = head . filter (\s -> length (foldl1 (\\) [s, n4, n7]) == 1) $ length6

		-- 6 can be found by looking for the string with length 1 after removing 5 and 1
		n6 = head . filter (\s -> length (foldl1 (\\) [s, n4, n7]) == 3) $ length6

		-- 0 can be found by looking for the string with length 0 after removing 6 and 9
		n0 = head . filter (\s -> length (foldl1 (\\) [s, n4, n7]) == 1) $ length6
	in M.fromList (zip [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] [0..])
	where filterLength l = filter (\s -> length s == l)