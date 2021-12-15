module Day14.Main (
	task1,
	task2,
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

task2 = "hoi"