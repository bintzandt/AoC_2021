module Day12.Main (
	task1,
	task2,
) where

import AOC
import Data.List.Split (splitOn)
import Data.Char (isUpper)
import Data.List (union)
import qualified Data.Set as S

data Node a = Big a | Small a deriving (Show, Eq, Ord)
type From = String
type To = String
type Edge = (From, To)

task1 = do
	input <- readAsListOfString "Day12/input.txt"
	let edges = map ((\(from:to:[]) -> (from,to)) . splitOn "-") input
	print $ length $ filter (not . null) $ findPath ["start"] edges


findPath :: [String] -> [Edge] -> [[String]]
findPath path@(n:ns) edges
  | n == "end" = [reverse path]
  | all (isUpper) n = findPaths
  | n `elem` ns = [[]]
  | otherwise = findPaths
  where
  	neighbours = union (map snd . filter (\(from,_) -> from == n) $ edges) ((map fst . filter (\(_,to) -> to == n) $ edges))
  	findPaths = concat . map (\n' -> findPath (n':path) edges) $ neighbours

containsDoubleSmallCave :: [String] -> Bool
containsDoubleSmallCave [] = False
containsDoubleSmallCave (n:ns)
  | all (isUpper) n = containsDoubleSmallCave ns
  | otherwise = (n `elem` ns) || (containsDoubleSmallCave ns)

findPath' :: [String] -> [Edge] -> [[String]]
findPath' path@(n:ns) edges
  | n == "start" && (not.null) ns = [[]]
  | n == "end" = [reverse path]
  | all (isUpper) n = findPaths
  -- Additional check to see if this is the first small cave we visit twice
  | n `elem` ns  && containsDoubleSmallCave ns = [[]]
  | otherwise = findPaths
  where
  	neighbours = union (map snd . filter (\(from,_) -> from == n) $ edges) ((map fst . filter (\(_,to) -> to == n) $ edges))
  	findPaths = concat . map (\n' -> findPath' (n':path) edges) $ neighbours

task2 = do
	input <- readAsListOfString "Day12/input.txt"
	let edges = map ((\(from:to:[]) -> (from,to)) . splitOn "-") input
	print $ length $ filter (not . null) $ findPath' ["start"] edges