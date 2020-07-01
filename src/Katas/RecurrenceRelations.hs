{-# LANGUAGE RankNTypes #-}
module Katas.RecurrenceRelations where
import Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.Map as M

type Solver a b = State (Mem a b) b

type Mem = M.Map

runSolver :: forall a b. Solver a b -> b
runSolver s = evalState s M.empty

solver :: forall a b. Ord a => (a -> Either b ([a], [b] -> b)) -> a -> Solver a b
solver f x = do
  m <- get
  case M.lookup x m of
    Just y -> return y
    Nothing ->
      case f x of
        Left y -> return y
        Right (as, fy) -> do
          ret <- fy <$> mapM (solver f) as
          M.insert x ret <$> get >>= put >> return ret

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = runSolver $ solver f n
