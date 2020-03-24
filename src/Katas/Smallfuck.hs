-- | Kata: Smallfuck - 5 kyu
{-# LANGUAGE TemplateHaskell #-}
module Katas.Smallfuck where

import Data.Map.Strict as M
import Control.Monad.State.Strict (State, execState, get, put)
import Control.Lens

type JumpTable = M.Map Int Int

data EvalState = EvalState
  { _code :: String
  , _tape :: String
  , _rip :: Int
  , _dp :: Int
  , _jumpTable :: JumpTable
  }
  deriving (Show, Eq)

makeLenses ''EvalState

type Eval = State EvalState

buildJT :: String -> JumpTable
buildJT src = go M.empty [] 0 src
  where go :: JumpTable -> [Int] -> Int -> String -> JumpTable
        go jt _ _ [] = jt
        go jt stack n ('[':xs) = go jt (n:stack) (n+1) xs
        go jt (p:ps) n (']':xs) = go (M.insert n p . M.insert p n $ jt) ps (n+1) xs
        go jt stack n (_:xs) = go jt stack (n+1) xs

moveDp :: Int -> Eval ()
moveDp = (dp.=)

moveDp' :: Int -> Eval () -- Move DP, relatively
moveDp' n = dp %= (+n)

moveDpL = moveDp' (-1)

moveDpR = moveDp' 1

jump :: Int -> Eval ()
jump = (rip.=)

jump' :: Int -> Eval () -- Move RIP, relatively
jump' = (rip%=) . (+)

step = jump' 1

flop :: Char -> Char
flop '0' = '1'
flop '1' = '0'

getDp :: Eval Int
getDp = use dp

getData :: Eval Char
getData = (!!) <$> getTape <*> getDp

getRip :: Eval Int
getRip = use rip

getTape :: Eval String
getTape = use tape

getCode :: Eval String
getCode = use code

flipOp :: Eval ()
flipOp = do
  dp_ <- getDp
  tape_ <- getTape
  tape %= replace dp_ (flop $ tape_ !! dp_)

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 v (x:xs) = v : xs
replace n v (x:xs) = x : replace (n-1) v xs

data Op = EOF | Op Char

fetchOp :: Eval Op
fetchOp = (,,,) <$> getCode <*> fmap length getTape <*> getRip <*> getDp >>= return . f
  where f (src, tlen, rip_, dp_)
          | dp_ < 0 || dp_ >= tlen = EOF
          | rip_ < 0 || rip_ >= length src = EOF
          | otherwise = Op $ src !! rip_

jmpOp :: Eval ()
jmpOp = (,) <$> use jumpTable <*> use rip >>= go
  where go :: (JumpTable, Int) -> Eval ()
        go (jt, x) = jump . fromMaybe $ M.lookup x jt

fromMaybe :: Maybe a -> a
fromMaybe (Just val) = val
fromMaybe Nothing = error "Unexpected Nothing!"

jmpWhen :: (Char -> Bool) -> Eval ()
jmpWhen pred = pred <$> getData >>= go
  where go True = jmpOp
        go False = return ()

jnzOp = jmpWhen (=='1')
jezOp = jmpWhen (=='0')

eval :: Eval ()
eval = fetchOp >>= go
  where go EOF = return ()
        go (Op ch) = runOp ch >> step >> eval
        runOp '>' = moveDpR
        runOp '<' = moveDpL
        runOp '*' = flipOp
        runOp '[' = jezOp
        runOp ']' = jnzOp
        runOp _ = return ()

eval1 :: Eval ()
eval1 = fetchOp >>= go
  where go EOF = return ()
        go (Op ch) = runOp ch >> step
        runOp '>' = moveDpR
        runOp '<' = moveDpL
        runOp '*' = flipOp
        runOp '[' = jezOp
        runOp ']' = jnzOp
        runOp _ = return ()

compile :: String -> String -> EvalState
compile code tape = EvalState
  { _code = code
  , _tape = tape
  , _rip = 0
  , _dp = 0
  , _jumpTable = buildJT code
  }

execEval :: EvalState -> Eval () -> String
execEval s e = execState e s ^. tape

run :: String -> String -> String
run = fmap (fmap $ flip execEval eval) compile

interpreter = run
