{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Katas.AdditionCommutes where

data Z
data S n

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n') = EqlS $ reflexive n'

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS ind) = EqlS $ symmetric ind

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS ind1) (EqlS ind2) = EqlS $ transitive ind1 ind2

aPlusO :: Natural a -> Equal a (a :+: Z)
aPlusO NumZ = reflexive NumZ
aPlusO (NumS n') = EqlS $ aPlusO n'

aPlusSb :: Natural a -> Natural (S b) -> Equal (a :+: S b) (S (a :+: b))
aPlusSb NumZ sb = reflexive sb
aPlusSb (NumS a) sb = EqlS $ aPlusSb a sb

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ b = transitive (reflexive b) (aPlusO b)
plusCommutes sa@(NumS a) b = transitive (EqlS $ plusCommutes a b) (symmetric $ aPlusSb b sa)

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:
{-

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-}
