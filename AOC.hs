module AOC (
	readAsListOfInt,
	readAsListOfString,
) where

readAsListOfInt file = do
	listOfStrings <- readAsListOfString file
	return $ map (\x -> read x :: Int) $ listOfStrings

readAsListOfString file = do
	raw <- readFile file
	return $ lines raw