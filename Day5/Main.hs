module Day5.Main (
	task1,
	task2,
) where

import AOC
import Data.List.Split
import qualified Data.Map as M

type X = Int
type Y = Int
type Count = Int
type Coordinate = (X, Y)
type Line = (Coordinate, Coordinate)

task1 = do
	input <- readAsListOfString "Day5/input.txt"
	let ls = filter isHorizontalOrVerticalLine $ toLines $ map (splitOn " -> ") input
	print $ M.size $ M.filter (>=2) (insertLines ls)

task2 = do
	input <- readAsListOfString "Day5/input.txt"
	let ls = toLines $ map (splitOn " -> ") input
	print $ M.size $ M.filter (>=2) (insertLines ls)

-- Insert each of the lines into a map using the from and to coordinate from the line.
-- Multiple occurances of the same coordinate increase the value in the map.
insertLines :: [Line] -> M.Map Coordinate Count
insertLines [] = M.empty
insertLines (line:ls) = M.unionWith (+) (M.fromListWith (+) (lineToList line)) (insertLines ls)

lineToList :: Line -> [(Coordinate, Count)]
lineToList line@((x1, y1), (x2, y2))
	| x1 == x2 = [((x1, y), 1) | y <- if y1 > y2 then [y2..y1] else [y1..y2]]
	| y1 == y2 = [((x, y1), 1) | x <- if x1 > x2 then [x2..x1] else [x1..x2]]
	| otherwise = [(c, 1) | c <- (diagonalLineToList line)]

diagonalLineToList :: Line -> [Coordinate]
diagonalLineToList (from@(x1, y1), to@(x2, y2))
	| from == to = [to]
	| otherwise = [from] ++ diagonalLineToList (newCoordinate,to)
	where
		newCoordinate = (if x1 < x2 then x1 + 1 else x1 -1, if y1 < y2 then y1 + 1 else y1 -1)

isHorizontalOrVerticalLine :: Line -> Bool
isHorizontalOrVerticalLine ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

toLines :: [[String]] -> [Line]
toLines [] = []
toLines (coordinates:xs) = (from, to):(toLines xs)
	where
		from = parseCoordinate (head coordinates)
		to = parseCoordinate (last coordinates)

parseCoordinate :: String -> Coordinate
parseCoordinate s = (x,y)
	where
		coordinateList = splitOn "," s
		x = read (head coordinateList) :: X
		y = read (last coordinateList) :: Y