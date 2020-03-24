{-# LANGUAGE TemplateHaskell #-}
module Katas.Imperative where

import Control.Monad.State (State, evalState, get, put, modify)
import Data.Map as M
import Control.Lens hiding ((+=), (-=), (*=))

data ImpState = ImpState
  { _rsp :: Integer
  , _stack :: M.Map Integer Integer
  } deriving (Eq, Show)

makeLenses ''ImpState

emptyState :: ImpState
emptyState = ImpState 0 M.empty

type Imp = State ImpState

newtype ImpVar = ImpVar { identify :: Integer }
  deriving (Eq, Show)

newtype Lit = Lit { getVal :: Integer }
  deriving (Eq, Show)

class Eval a where
  eval :: a -> Imp Integer

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "Woops! There's something wrong!"

evalVar :: ImpVar -> Imp Integer
evalVar (ImpVar idx) = fromMaybe . M.lookup idx <$> use stack

evalLit :: Lit -> Imp Integer
evalLit = return . getVal

instance Eval ImpVar where
  eval = evalVar

instance Eval Lit where
  eval = evalLit

def :: Eval v => Imp v -> Integer
def = flip evalState emptyState . (>>=eval)

var :: Integer -> Imp ImpVar
var v = rsp %= (+1) >> use rsp >>= go
  where go :: Integer -> Imp ImpVar
        go i = stack %= M.insert i v >> return (ImpVar i)

lit :: Integer -> Lit
lit = Lit

while :: ImpVar -> (Integer -> Bool) -> Imp a -> Imp ()
while t pred act = (>>=go) . fmap pred $ eval t
  where go True = act >> while t pred act
        go False = return ()

mkOp :: Eval v => (Integer -> Integer -> Integer) -> ImpVar -> v -> Imp ()
mkOp op v@(ImpVar vid) val = op <$> eval v <*> eval val >>= f
  where f :: Integer -> Imp ()
        f v' = stack %= M.insert vid v'

(+=) :: Eval v => ImpVar -> v -> Imp ()
(+=) = mkOp (+)

(-=) :: Eval v => ImpVar -> v -> Imp ()
(-=) = mkOp (-)

(*=) :: Eval v => ImpVar -> v -> Imp ()
(*=) = mkOp (*)
