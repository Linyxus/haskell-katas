module Katas.FixIt where

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f [] = []
reverse' f (x:xs) = f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' _ _ x [] = x
foldr' f g m (x:xs) = g x (f g m xs)
