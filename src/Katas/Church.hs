{-# LANGUAGE RankNTypes #-}
module Katas.Church where

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr a) (Nr b) = Nr $ \ s z -> a s (b s z)

mult :: Number -> Number -> Number
mult (Nr a) (Nr b) = Nr $ \ s z -> a (b s) z

pow :: Number -> Number -> Number
pow (Nr x) (Nr n) = Nr $ \ s -> n x s
