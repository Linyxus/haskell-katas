-- | Kata: Snail - 4 kyu
module Katas.Snail where

import Debug.Trace

snail :: [[Int]] -> [Int]
snail m = trace (show m) $ travel m

borderN :: Int -> [(Int, Int)]
borderN n = [(0, x) | x <- [0..n-1]] ++ [(x, n-1) | x <- [1..n-1]]
            ++ [(n-1, x) | x <- reverse [0..n-2]] ++ [(x, 0) | x <- reverse [1..n-2]]

travel :: [[Int]] -> [Int]
travel [] = []
travel [[]] = []
travel [[x]] = [x]
travel m = ((\(x,y) -> m !! x !! y) <$> borderN (length m)) ++ travel (inner m)

inner :: [[a]] -> [[a]]
inner = fmap (dropFirst . dropLast) . dropFirst . dropLast

dropFirst :: [a] -> [a]
dropFirst = drop 1

dropLast :: [a] -> [a]
dropLast [x] = []
dropLast [] = []
dropLast (x:xs) = x : dropLast xs
