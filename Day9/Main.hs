module Day9.Main (
  task1,
  task2,
) where

import AOC
import Data.Char (digitToInt)
import Data.List (sort)
import Debug.Trace (trace)
import qualified Data.Set as S

type Pos = (Int, Int)

task1 = do
  input <- readAsListOfString "Day9/input.txt"
  let tubes = map parseAsInt input
  let positions = [(x, y) | x <- [0..((length (tubes !! 0))- 1)], y <- [0..(length tubes - 1)]]
  print $ (\list -> sum list + length list) . map (getValue tubes) . filter (isLowPoint tubes) $ positions


getValue :: [[Int]] -> Pos -> Int
getValue list (x, y) = (list !! y) !! x

-- Returns true if the first point is lower than all other points
comparePoints :: [Pos] -> [[Int]] -> Bool
comparePoints (x:[]) _ = True
comparePoints (x:y:ys) list = getTubeValue x < getTubeValue y && comparePoints (x:ys) list
  where
    getTubeValue = getValue list

isLowPoint :: [[Int]] -> Pos -> Bool
isLowPoint tubes point = comparePoints (point:getNeighbouringPoints tubes point) tubes
  
getNeighbouringPoints :: [[Int]] -> Pos -> [Pos]
getNeighbouringPoints tubes (x,y)
  -- Upper left corner
  | x == 0 && y == 0 = [(x+1,y), (x, y+1)]
  -- Upper right corner
  | x ==  maxX && y == 0 = [(x-1, y), (x, y+1)]
  -- Lower left corner
  | x == 0 && y == maxY = [(x+1, y), (x, y-1)]
  -- Lower right corner
  | x == maxX && y == maxY = [(x-1, y), (x, y-1)]
  -- Last in row
  | x == maxX = [(x-1, y), (x, y+1), (x, y-1)]
  -- Last in column
  | y == maxY = [(x-1, y), (x+1, y), (x, y-1)]
  -- First in row
  | x == 0 = [(x+1, y), (x, y-1), (x, y+1)]
  -- First in column
  | y == 0 = [(x+1, y), (x-1, y), (x, y+1)]
  -- All other positions
  | otherwise = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where
    maxX = (length (tubes !! y)) - 1
    maxY = (length tubes) - 1


parseAsInt :: String -> [Int]
parseAsInt [] = []
parseAsInt (x:xs) = (digitToInt x):(parseAsInt xs)

task2 = do
  input <- readAsListOfString "Day9/input.txt"
  let tubes = map parseAsInt input
  let positions = [(x, y) | x <- [0..((length (tubes !! 0))- 1)], y <- [0..(length tubes - 1)]]
  let lowPoints = filter (isLowPoint tubes) positions
  print $ (product . take 3 . reverse . sort) $ map S.size $ map (\low -> findNon9Neighbours ([low]) tubes S.empty S.empty) lowPoints

findNon9Neighbours :: [Pos] -> [[Int]] -> S.Set Pos -> S.Set Pos -> S.Set Pos
findNon9Neighbours [] _ _ result = result
findNon9Neighbours (pos:ps) tubes checked result
  | S.member pos checked = findNon9Neighbours ps tubes checked result
  | otherwise = let
    neighbours = getNeighbouringPoints tubes pos
    non9 = filter ((/=9).getValue tubes) $ neighbours
    in findNon9Neighbours (ps++non9) tubes (S.insert pos checked) (S.union result (S.fromList non9))



-- findBasins :: [[Int]] -> [Pos] -> [[Pos]]
-- findBasins tubes (low:lows) = let
  -- in [neighbours]