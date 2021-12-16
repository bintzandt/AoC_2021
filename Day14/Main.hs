module Day14.Main (
	task1,
	task2,
	createPolymer,
) where

import AOC
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Sequence = M.Map (Char, Char) Char

task1 = do
	input <- readAsListOfString "Day14/input.txt"
	let (start, mapping) = (head input, createMapping $ drop 2 input)
	let result = runProcess 10 mapping start
	let numbers = map snd . M.toList . M.fromListWith (+) $ (map (\c -> (c, 1)) result)
	print $ (maximum numbers) - (minimum numbers)


snd' :: [(Char, Int)] -> [Int]
snd' list = map snd list

runProcess :: Int -> Sequence -> String -> String
runProcess 0 _ string = string
runProcess c mapping string = runProcess (c-1) mapping (runString mapping string)

runString :: Sequence -> String -> String
runString mapping (x:y:[]) = newString
	where
		newString = case M.lookup (x,y) mapping of
			Just c -> x:c:y:[]
			Nothing -> x:y:[]
runString mapping (x:y:rest) = newString ++ runString mapping (y:rest)
	where
		newString = case M.lookup (x,y) mapping of
			Just c -> x:c:[]
			Nothing -> x:[]

createMapping :: [String] -> Sequence
createMapping [] = M.empty
createMapping (l:ls) = M.union (M.insert (head from, last from) (head to) M.empty) (createMapping ls)
	where
		withoutSpaces = filter (not . (`elem` " ")) l
		[from, to] = splitOn "->" withoutSpaces

type Count = Int
type Rules = M.Map String [String]
type Polymer = M.Map String Count

createRules :: [String] -> Rules
createRules [] = M.empty
createRules (l:ls) = M.union (M.insert from [(head from):(head to):[], (head to):(last from):[]] M.empty) (createRules ls)
		where
			withoutSpaces = filter (not . (`elem` " ")) l
			[from, to] = splitOn "->" withoutSpaces

createPolymer :: String -> Polymer
createPolymer [] = M.empty
createPolymer (x:[]) = M.empty
createPolymer (x:y:rest) = M.unionWith (+) (M.fromList [((x:y:[]), 1)]) (createPolymer (y:rest))

runPolymerisation :: Int -> Rules -> Polymer -> Polymer
runPolymerisation 0 _ poly = poly
runPolymerisation x rules poly = runPolymerisation (x-1) rules newPoly
	where
		newPoly = M.foldlWithKey (\newMap key count -> M.unionWith (+) newMap (M.fromList (map (\connection -> (connection, count)) ((M.!) rules key)))) M.empty poly

task2 = do
	input <- readAsListOfString "Day14/input.txt"
	let (start, rules) = (head input, createRules $ drop 2 input)
	let finalPoly = runPolymerisation 40 rules $ createPolymer start
	let countMap = M.foldlWithKey (\countMap key count -> M.unionWith (+) countMap (M.fromListWith (+) (map (\c -> (c, count)) key))) M.empty finalPoly
	let correctedCountMap = M.unionWith (+) (M.map (`div` 2) countMap) (M.fromList [(head start,1), (last start,1)])
	let counts = map snd $ M.toList correctedCountMap
	print (maximum counts - minimum counts)
	