module Katas.ShortestList where

shortestList :: [[a]] -> [a]
shortestList [] = []
shortestList xs = go xs xs
  where go xs ys = f xs ys $ findEmpty xs ys
        f _ _ (Just x) = x
        f xs ys Nothing = go (step xs) ys

step :: [[a]]
     -> [[a]]
step []          = []
step ([]:xs)     = [] : step xs
step ((_:xs):ys) = xs : step ys

findEmpty :: [[a]] -- current
          -> [[a]] -- original
          -> Maybe [a] -- result
findEmpty [] _          = Nothing
findEmpty ([]:_) (xs:_) = pure xs
findEmpty (_:xs) (_:ys) = findEmpty xs ys


