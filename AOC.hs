module AOC (
	readAsListOfInt,
	readAsListOfString,
	readStringAsListOfInt,
) where

import Data.List.Split

-- Useful when the input is a file with multiple lines
-- with exactly one Int per line.
-- Each line becomes an entry in an array, parsed as Int.
readAsListOfInt file = do
	listOfStrings <- readAsListOfString file
	return $ map (\x -> read x :: Int) $ listOfStrings

-- Useful when the input is a file with multiple lines.
-- Each line becomes an entry in an array.
readAsListOfString file = do
	raw <- readFile file
	return $ lines raw

-- Useful when the input is a single string with Ints.
-- Each Int becomes an entry in an array.
readStringAsListOfInt file = do
	input <- readFile file
	return $ map (\x -> read x :: Int) $ splitOn "," input