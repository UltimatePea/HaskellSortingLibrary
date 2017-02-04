
module Sort (
    mergeSort
    ) where

import Data.List (delete)
mergeStep :: [Int] -> [Int] -> [Int]
mergeStep [] b = b
mergeStep a [] = a
mergeStep (x:xs) (y:ys) = if x < y then x:mergeStep xs (y:ys) 
                            else y:mergeStep (x:xs) ys

split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split list = let half = (length list) `div` 2
             in (take half list, drop half list)


mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = let pair = split list
                 in mergeStep (mergeSort (fst pair)) (mergeSort (snd pair))

selections :: [a] -> [(a, [a])]
selections [] = []
selections (x:xs) = (x,xs) : [(y, x:ys) | (y,ys) <- selections xs]

permute ::[a] -> [[a]]
permute [x] = [[x]]
permute xs = [y:zs |(y,ys) <- selections xs, zs <- permute ys]

isSorted :: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (x:xs) = x < head xs && isSorted xs

permuteSort :: Ord a=> [a] -> [a]
permuteSort xs = head (filter isSorted (permute xs)) 

findMax :: Ord a=> [a] -> a
findMax [x] = x
findMax (x:xs) = let tailMax = findMax xs
                 in if x>tailMax then x else tailMax


findMin :: Ord a=> [a] -> a
findMin [x] = x
findMin (x:xs) = let tailMin = findMin xs
                 in if x<tailMin then x else tailMin

selectionSort :: Ord a=> [a] -> [a]
selectionSort [x] = [x]
selectionSort xs  = let min = findMin xs
                   in min : selectionSort (delete min xs)
