{-# Language RankNTypes #-}
module Katas.ChurchBooleans where

import Prelude hiding (Bool,False,True,not,and,or,(&&),(||),(==),(/=))

type Boolean = forall a. a -> a -> a -- this requires RankNTypes

false, true :: Boolean
false t f = f
true t f = t

not :: Boolean -> Boolean
not g t f = g f t

and :: Boolean -> Boolean -> Boolean
and g h t f = g (h t f) f

or :: Boolean -> Boolean -> Boolean
or g h t f = g t $ h t f

xor :: Boolean -> Boolean -> Boolean
xor g h = g (not h) h
