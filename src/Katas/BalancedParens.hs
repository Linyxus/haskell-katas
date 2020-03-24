-- | Kata: Balanced Parens - 4 kyu
module Katas.BalancedParens where

balancedParens :: Int -> [String]
balancedParens = dfs

dfs :: Int -> [String]
dfs 0 = [""]
dfs n = [0..n-1] >>= f
  where f i = fmap g $ (,) <$> dfs i <*> dfs (n - 1 - i)
        g (a, b) = "(" ++ a ++ ")" ++ b
