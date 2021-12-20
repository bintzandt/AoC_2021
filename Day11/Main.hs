module Day11.Main (
	task1,
	task2,
	neighbours,
) where

import AOC
import qualified Data.Map as M
import Data.Char (digitToInt)

type Pos = (Int, Int)
type Grid = [[Int]]

neighbours :: Pos -> [Pos]
-- Upper left
neighbours (0,0) = [(1, 0), (1, 1), (0, 1)]
-- Upper right
neighbours (9,0) = [(8,0), (8,1), (9,1)]
-- Lower left
neighbours (0,9) = [(0,8), (1,8), (1,9)]
-- Lower right
neighbours (9,9) = [(8,9), (8,8), (9,8)]
-- Left row
neighbours (0,y) = [(1, y), (1, y-1), (1, y+1), (0, y-1), (0, y+1)]
-- Top row
neighbours (x,0) = [(x-1, 0), (x-1, 1), (x, 1), (x+1, 1), (x+1, 0)]
-- Right row
neighbours (9,y) = [(9,y-1), (8,y-1), (8,y), (8,y+1), (9,y+1)]
-- Bottom row
neighbours (x,9) = [(x-1, 9), (x-1, 8), (x, 8), (x+1, 8), (x+1, 9)]
-- Other pos
neighbours (x,y) = [(x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1), (x+1, y), (x+1, y+1), (x,y+1), (x-1, y+1)]

posValue :: Grid -> Pos -> Int
posValue grid (x,y) = (grid !! y) !! x

runFlash :: Grid -> Pos -> M.Map Pos [Pos] -> Grid
runFlash grid pos mapping
  | posValue grid pos == 8 = foldl (\grid' pos' -> runFlash grid' pos' mapping) newGrid neighbours
  | posValue grid pos == 9 = grid
  | otherwise = newGrid
  where
  	neighbours = (M.!) mapping pos
  	newGrid = incrementValueAtPos grid pos

incrementValueAtPos :: Grid -> Pos -> Grid
incrementValueAtPos grid (x,y) = updatedGrid
  where
  	row = grid !! y
  	updatedRow = take x row ++ [((row !! x) + 1) `mod` 10] ++ (reverse . take (9-x) . reverse $ row)
  	updatedGrid = take y grid ++ [updatedRow] ++ (reverse . take (9-y) . reverse $ grid)


task1 = do
	input <- readAsListOfString "Day11/input.txt"
	let grid = map (map digitToInt) input
	let pointsMap = M.fromList [((x,y), neighbours (x,y)) | x <- [0..9], y <- [0..9]]
	print $ grid
	print ""
	print $ runFlash grid (2,0) pointsMap

task2="hoi"