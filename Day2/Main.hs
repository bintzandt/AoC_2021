module Day2.Main (
  task1,
  task2,
) where

import AOC

task1 = do
  input <- readAsListOfString "Day2/input.txt"
  return $ calcPosition 0 0 $ map (\x -> (head x, read (last x) :: Int)) $ map words $ input

task2 = do
  input <- readAsListOfString "Day2/input.txt"
  return $ calcPosition' 0 0 0 $ map (\x -> (head x, read (last x) :: Int)) $ map words $ input

calcPosition' hor depth aim [] = hor * depth
calcPosition' hor depth aim (("down", number):xs) = calcPosition' hor depth (aim+number) xs
calcPosition' hor depth aim (("up", number):xs) = calcPosition' hor depth (aim-number) xs
calcPosition' hor depth aim (("forward", number):xs) = calcPosition' (hor+number) (depth+(aim*number)) aim xs

calcPosition hor depth [] = hor * depth
calcPosition hor depth (("forward", number):xs) = calcPosition (hor+number) depth xs
calcPosition hor depth (("down", number):xs) = calcPosition hor (depth+number) xs
calcPosition hor depth (("up", number):xs) = calcPosition hor (depth-number) xs