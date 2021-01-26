-- | Kata: Coroutine - 1 kyu
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Katas.Coroutine where
import           Control.Monad (ap, forever)

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a
  = Done a
  | Out d (Coroutine r u d a)
  | In (u -> Coroutine r u d a) deriving Functor

apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

joinCoroutine :: Coroutine r u d (Coroutine r u d a) -> Coroutine r u d a
joinCoroutine (Coroutine cont) = Coroutine $ \ku -> cont (km ku)
  where km ku (Done cd)  = runCoroutine cd ku
        km ku (Out d kd) = ku . Out d $ joinCoroutine kd
        km ku (In c)     = ku . In $ joinCoroutine <$> c

instance Monad (Coroutine r u d) where
  return x = Coroutine ($ Done x)
  f >>= g  = joinCoroutine $ g <$> f

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $ \k -> apply p2 (k2 k p1)
  where k2 k _ (Done a)      = k $ Done a
        k2 k p1 (Out d cont) = k $ Out d (p1 >>> cont)
        k2 k p1 (In cont)    = apply p1 (k1 k cont)
        k1 k _ (Done a)     = k $ Done a
        k1 k fp2 (Out d p') = apply (fp2 d) (k2 k p')
        k1 k fp2 (In fp)    = k . In $ flip pipe2 fp2 <$> fp

pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
p1 `pipe2` fp2 = Coroutine $ \k -> apply p1 (k1 k fp2)
  where k1 k fp2 (Done a)   = k $ Done a
        k1 k fp2 (In fp1)   = k . In $ g <$> fp1
        k1 k fp2 (Out d p1) = runCoroutine (p1 >>> fp2 d) k
        g p1 = p1 `pipe2` fp2

output :: a -> Coroutine r u a ()
output v = Coroutine ($ x)
  where x = Out v $ return ()

input :: Coroutine r v d v
input = Coroutine ($ x)
  where x = In return

produce :: [a] -> Coroutine r u a ()
produce [] = return ()
produce (x:xs) = Coroutine ($ c)
  where c = Out x $ produce xs

consume :: Coroutine [t] u t a -> [t]
consume = flip runCoroutine k
  where k (Done _)     = []
        k (Out d cont) = d : consume cont
        k (In _)       = error "can not feed value into a consumed coroutine"

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = Coroutine f
  where f k = k (In $ fc k)
        fc k u | p u = output u >> filterC p
               | otherwise = filterC p

limit :: Int -> Coroutine r v v ()
limit 0 = pure ()
limit n = Coroutine f
  where f k = k (In $ fc k)
        fc k u = output u >> limit (n - 1)

suppress :: Int -> Coroutine r v v ()
suppress 0 = filterC $ const True
suppress n = Coroutine f
  where f k = k $ In fc
        fc _ = suppress (n - 1)

add :: Coroutine r Int Int ()
add = Coroutine f
  where f k = k $ In add1


add1 :: Int -> Coroutine r Int Int ()
add1 x = Coroutine f
  where f k = k $ In g
        g y = output (x + y) >> add

duplicate :: Coroutine r v v ()
duplicate = Coroutine f
  where f k = k $ In g
        g x = output x >> output x >> duplicate

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5

triNum :: [Int]
triNum = f <$> [1..]
  where f n = sum [1..n]

p2 = produce triNum

p3 = duplicate >>> add

p4 = duplicate >>> suppress 1 >>> add

-- >>> consume $ return 1 >>= output
-- [1]
