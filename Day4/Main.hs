module Day4.Main (
	task1,
	task2,
) where

import AOC
import Data.List
import Data.List.Split

type Entry = (Int, Bool)
type Row = [Entry]
type Board = [Row]

task1 = do
	input <- readAsListOfString "Day4/input.txt"
	let (numbers, boards) = (map (\x -> read x :: Int) (splitOn "," (head input)), createBoards $ drop 2 input)
	print $ runBingo numbers boards

task2 = do
	input <- readAsListOfString "Day4/input.txt"
	let (numbers, boards) = (map (\x -> read x :: Int) (splitOn "," (head input)), createBoards $ drop 2 input)
	print $ runReverseBingo numbers boards

runReverseBingo :: [Int] -> [Board] -> Int
runReverseBingo [] _ = 0
runReverseBingo _ [] = 0
runReverseBingo (x:numbers) boards = if length updatedBoards == 1  && bingo (head updatedBoards) then result else runReverseBingo numbers nonBingoBoards
	where
		updatedBoards = map (updateBoard x) boards
		result = calculateSumOfUnmarkedNumbers (head updatedBoards) * x
		nonBingoBoards = filter (\board -> not $ bingo board) updatedBoards

runBingo :: [Int] -> [Board] -> Int
runBingo [] _ = 0
runBingo _ [] = 0
runBingo (x:numbers) boards = if any bingo updatedBoards then result else runBingo numbers updatedBoards
	where
		updatedBoards = map (updateBoard x) boards
		result = case find bingo updatedBoards of
			Nothing -> -1
			Just board -> calculateSumOfUnmarkedNumbers board * x

calculateSumOfUnmarkedNumbers :: Board -> Int
calculateSumOfUnmarkedNumbers [] = 0
calculateSumOfUnmarkedNumbers (row:rest) = result + calculateSumOfUnmarkedNumbers rest
	where result = sum $ map fst $ filter (\(x,y) -> y == False) row

bingo :: Board -> Bool
bingo board = foldl (\acc row -> acc || bingoRow row) False board || foldl (\acc row -> acc || bingoRow row) False (transpose board)

bingoRow :: Row -> Bool
bingoRow row = foldl (\acc (x, y) -> acc && y) True row

updateBoard :: Int -> Board -> Board
updateBoard number board = map (updateRow number) board

updateRow :: Int -> Row -> Row
updateRow number row = [(x, found || number == x) | (x, found) <- row]

createBoards :: [String] -> [Board]
createBoards [] = []
createBoards xs
	| length xs < 5 = []
	| otherwise = finishedBoard : createBoards (drop 6 xs)
	where
		boardString = take 5 xs
		seperateNumbers = map words boardString
		finishedBoard = map (\row -> zip [read x :: Int | x <- row] (repeat False)) seperateNumbers