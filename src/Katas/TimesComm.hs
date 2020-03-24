{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}
module Katas.TimesComm where

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

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n') = EqlS $ reflexive n'

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq') = EqlS $ symmetric eq'

plus :: Natural a -> Natural b -> Natural (a :+: b)
plus NumZ b = b
plus (NumS a') b = NumS $ plus a' b

-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ b c = reflexive $ plus b c
plusAssoc (NumS a') b c = EqlS $ plusAssoc a' b c

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
plusComms :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComms NumZ b = transitive (reflexive b) (aPlusO b)
plusComms sa@(NumS a) b = transitive (EqlS $ plusComms a b) (symmetric $ aPlusSb b sa)

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS a') = zeroComm a'

leftPlusEql :: Natural a -> Natural b -> Natural c -> Equal b c -> Equal (a :+: b) (a :+: c)
leftPlusEql NumZ _ _ eq = eq
leftPlusEql (NumS a') b c eq = EqlS $ leftPlusEql a' b c eq

rightPlusEql :: Natural a -> Natural b -> Natural c -> Equal b c -> Equal (b :+: a) (c :+: a)
rightPlusEql a b c eq = transitive (plusComms b a) $ transitive (leftPlusEql a b c eq) (plusComms a c)

mult :: Natural n -> Natural m -> Natural (n :*: m)
mult NumZ _ = NumZ
mult (NumS n') m = plus m $ mult n' m

natPred :: Natural (S m) -> Natural m
natPred (NumS m') = m'

nTimesSm :: Natural n -> Natural (S m) -> Equal (n :*: S m) (n :+: (n :*: m))
nTimesSm NumZ _ = EqlZ
nTimesSm n@(NumS n') sm = hypo''
  where m = natPred sm
        applyInd = leftPlusEql sm (mult n' sm) (plus n' (mult n' m)) $ nTimesSm n' sm
        smPlusnSymm = EqlS $ plusComms m n'
        hypo = rightPlusEql (mult n' m) (NumS (plus m n')) (NumS (plus n' m)) smPlusnSymm
        hypoL = plusAssoc sm n' (mult n' m)
        hypoR = symmetric $ plusAssoc n m (mult n' m)
        hypo' = transitive hypoL $ transitive hypo hypoR
        hypo'' = transitive applyInd hypo'

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ b = transitive EqlZ (zeroComm b)
timesComm a@(NumS a') b = symmetric $ transitive hypo hypoInd
  where hypo = nTimesSm b a
        hypoInd = leftPlusEql b (mult b a') (mult a' b) $ symmetric $ timesComm a' b
