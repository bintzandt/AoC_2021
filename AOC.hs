module AOC (
	readAsListOfInt
) where

readAsListOfInt file = do
	raw <- readFile file
	return $ map (\x -> read x :: Int) $ lines raw