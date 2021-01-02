-- Kata: Finally tagless interpreter, 2 kyu
{-# LANGUAGE RankNTypes #-}
module Katas.Tagless where
import Prelude hiding (and, or)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)

  loop   :: r h (a -> a) -> r h a

  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal

  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool

  ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term

newtype R h a = R { unR :: h -> a }

instance Language R where
  here = R f
    where f (a, _) = a
  before v = R f
    where f (_, r) = unR v r
  lambda e = R f
    where f env a = unR e (a, env)
  apply func x = R f
    where f env = unR func env $ unR x env

  loop func = apply func (loop func)

  int = R . const
  add x y = R $ \env -> unR x env + unR y env
  down = add $ int (-1)
  up = add $ int 1
  mult x y = R $ \env -> unR x env * unR y env
  gte l r = R $ \env -> unR l env >= unR r env

  bool = R . const
  and a b = R $ \env -> unR a env && unR b env
  or a b = R $ \env -> unR a env || unR b env
  neg = R . fmap not . unR

  ifte cond t f = R $ \env -> if unR cond env then unR t env else unR f env

type Term a = forall r h . Language r => r h a

interpret :: Term a -> a
interpret t = unR t ()

-- ieq :: Term (Int -> Int -> Bool)
-- ieq = lambda $ lambda $ and (gte here (before here))
--                             (gte (before here) here)

-- eq0 :: Term (Int -> Bool)
-- eq0 = apply ieq (int 0)

-- fact_ :: Term ((Int -> Int) -> (Int -> Int))
-- fact_ = lambda $ lambda $ ifte (apply eq0 here)
--                                (int 1)
--                                (mult here (apply (before here) (down here)))

-- fact :: Term (Int -> Int)
-- fact = loop fact_
