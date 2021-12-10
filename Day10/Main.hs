module Day10.Main (
	task1,
	task2,
) where

import AOC
import Data.List (isInfixOf, find, sort, isPrefixOf)

data Line = Corrupted Char | Incomplete String deriving Show

parse :: [String] -> [Line]
parse = map (parseLine . compact)

-- Removes all legal combinations of brackets from the string
compact :: String -> String
compact str
  | any (`isInfixOf` str) legals = compact (removeAll legals str)
  | otherwise = str
  where
  	legals = ["<>", "{}", "()", "[]"]
	removeAll strings string = foldl (flip remove) string strings

remove :: Eq a => [a] -> [a] -> [a]
remove _ [] = []
remove w s@(c:cs)
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs

-- Since all legal combinations have been removed from the string at this point,
-- we can simple check for occurences of a closing tag. If one exists the string is corrupt,
-- otherwise the string is incomplete.
parseLine :: String -> Line
parseLine line =
	case find (`elem` [')', ']', '>', '}']) line of
		Just s -> Corrupted s
		Nothing -> Incomplete line

corruptedScore :: Char -> Int
corruptedScore c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = 0

incompleteScore :: Char -> Int
incompleteScore c
  | c == '(' = 1
  | c == '[' = 2
  | c == '{' = 3
  | c == '<' = 4
  | otherwise = 0

task1 = do
	input <- readAsListOfString "Day10/input.txt"
	let parsed = parse input
	print $ sum [corruptedScore c | Corrupted c <- parsed]

task2 = do
	input <- readAsListOfString "Day10/input.txt"
	let parsed = parse input
	let computedScores = [computeScore x | Incomplete x <- parsed]
	print $ middle computedScores
	where
		computeScore = foldr (\c acc -> acc * 5 + incompleteScore c) 0
		middle scores = sort scores !! ((length scores) `div` 2)