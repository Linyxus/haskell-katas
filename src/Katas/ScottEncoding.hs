{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module Katas.ScottEncoding where
import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))
import Data.Functor (Functor(..))
import Data.Bifunctor (Bifunctor(..), first, second)

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }

newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }

newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }

newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

instance Functor SMaybe where
  fmap f (SMaybe matchMaybe) = SMaybe $ \b m -> matchMaybe b (m . f)

instance Bifunctor SPair where
  bimap f g (SPair matchPair) = SPair matchPair'
    where matchPair' elim = matchPair $ \ a c -> elim (f a) (g c)

emptyList :: SList a
emptyList = SList const

consList :: a -> SList a -> SList a
consList a xs = SList $ \_ step -> step a xs

toPair :: SPair a b -> (a,b)
toPair (SPair f) = f (,)

fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b

fst :: SPair a b -> a
fst = f . toPair
  where f (a, b) = a

snd :: SPair a b -> b
snd = f . toPair
  where f (a, b) = b

swap :: SPair a b -> SPair b a
swap = fromPair . f . toPair
  where f (a, b) = (b, a)

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ SPair g
  where g func = func a b

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f (SPair func) = func f

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe match) = match Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe const
fromMaybe (Just x) = SMaybe $ \ _ f -> f x

isJust :: SMaybe a -> Bool
isJust (SMaybe match) = match False $ const True

isNothing :: SMaybe a -> Bool
isNothing = not . isJust

catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList recList) = recList base step
  where base = SList const
        step (SMaybe matchMaybe) (SList recList') =
          let rem = recList' base step
              f x = SList $ \ _ g -> g x rem
          in matchMaybe rem f

toEither :: SEither a b -> Either a b
toEither (SEither m) = m Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither $ \ f _ -> f a
fromEither (Right b) = SEither $ \ _ g -> g b

isLeft :: SEither a b -> Bool
isLeft (SEither m) = m (const True) (const False)

isRight :: SEither a b -> Bool
isRight = not . isLeft

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition (SList recList) = recList base step
  where base = SPair $ \f -> f emptyList emptyList
        step (SEither matchEither) (SList recList') =
          let rem = recList' base step
              fl a = first (consList a) rem
              fr b = second (consList b) rem
          in matchEither fl fr

toList :: SList a -> [a]
toList (SList recList) = recList [] step
  where step x (SList recList') = x : recList [] step

fromList :: [a] -> SList a
fromList = undefined

cons :: a -> SList a -> SList a
cons = error "cons"

concat :: SList a -> SList a -> SList a
concat = error "concat"

null :: SList a -> Bool
null = error "null"

length :: SList a -> Int
length = error "length"

map :: (a -> b) -> SList a -> SList b
map = error "map"

zip :: SList a -> SList b -> SList (SPair a b)
zip = error "zip"

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl = error "foldl"

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr = error "foldr"

take :: Int -> SList a -> SList a
take = error "take"

