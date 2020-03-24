-- | Kata: Fibonacci - 3 kyu
module Katas.Fibonacci where

type Vec = (Integer, Integer)
type Mat = (Integer, Integer, Integer, Integer)

pMat :: Mat
pMat = (1, 1, 1, 0)

pMat_ :: Mat
pMat_ = (0, 1, 1, -1)

v0 :: Vec
v0 = (0, 1)

vecmul :: Mat -> Vec -> Vec
vecmul (a11, a12, a21, a22) (x1, x2) = (a11 * x1 + a12 * x2, a21 * x1 + a22 * x2)

matmul :: Mat -> Mat -> Mat
matmul (a11, a12, a21, a22) (b11, b12, b21, b22) =
  ( a11 * b11 + a12 * b21
  , a11 * b12 + a12 * b22
  , a21 * b11 + a22 * b21
  , a21 * b12 + a22 * b22
  )

iMat = (1, 0, 0, 1)

fastExp :: Mat -> Integer -> Mat
fastExp _ 0 = iMat
fastExp m n
  | even n = let m' = fastExp m (n `div` 2) in matmul m' m'
  | otherwise = matmul m $ fastExp m (n-1)

fib :: Integer -> Integer
fib n = fst $ fibVec n

fibVec :: Integer -> Vec
fibVec 0 = v0
fibVec n
  | n > 0 = vecmul (fastExp pMat n) v0
  | n < 0 = vecmul (fastExp pMat_ (-n)) v0
