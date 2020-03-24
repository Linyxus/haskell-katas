-- | Kata: Befunge93 - 4 kyu
module Katas.Befunge93 where

import System.Random (StdGen, mkStdGen, randomRIO, randomR, getStdGen)
import Control.Monad.State (StateT, execStateT, lift, get, put, modify)
import Control.Monad.Writer (Writer, runWriter, tell)
import qualified Data.Map as M
import Data.Char (ord, chr)
import Control.Monad (liftM3)
import Debug.Trace

type Eval = StateT EvalState (Writer String)

data EvalState = EvalState
  { evalSource :: SourceMap
  , evalPos :: (Int, Int)
  , evalStack :: Stack Int
  , evalDir :: EvalDir
  , stdGen :: StdGen
  , evalMode :: EvalMode
  }

type Stack a = [a]

popS :: Stack Int -> Stack Int
popS [] = []
popS s = tail s

pushS :: Int -> Stack Int -> Stack Int
pushS = (:)

topS :: Stack Int -> Int
topS [] = 0
topS s = head s

emptyS :: Stack Int -> Bool
emptyS = (==[])

type SourceMap = [String]

data EvalMode = NormalMode | StringMode
  deriving (Eq, Show)

data EvalDir = L | R | U | D
  deriving (Eq, Enum, Bounded, Show)

range :: [EvalDir]
range = [minBound .. maxBound]

emptyState :: EvalState
emptyState = EvalState [] (0, 0) [] R undefined NormalMode

execEval :: EvalState -> Eval a -> String
execEval s v = snd . runWriter $ execStateT v s

runEval :: EvalState -> Eval a -> (EvalState, String)
runEval s v = runWriter $ execStateT v s

empty :: Eval Bool
empty = emptyS . evalStack <$> get

pop :: Eval Int
pop = topS . evalStack <$> get <* (modify $ \x -> x { evalStack = popS $ evalStack x })

push :: Int -> Eval ()
push v = modify $ \x -> x { evalStack = pushS v $ evalStack x }

binOp :: (Int -> Int -> Int) -> Eval ()
binOp op = f >>= push
  where f = op <$> pop <*> pop

addOp :: Eval ()
addOp = binOp (+)

subOp :: Eval ()
subOp = binOp $ flip (-)

mulOp :: Eval ()
mulOp = binOp (*)

divOp :: Eval ()
divOp = binOp $ flip g
  where f _ 0 = 0
        f x y = x `div` y
        g x y -- rounding down
          | x < 0 = f (x-1) y
          | otherwise = f x y

modOp :: Eval ()
modOp = binOp $ flip f
  where f _ 0 = 0
        f x y = x `mod` y

gtOp :: Eval ()
gtOp = binOp f
  where f x y
          | x < y = 1
          | otherwise = 0

uOp :: (Int -> Int) -> Eval ()
uOp f = f <$> pop >>= push

notOp :: Eval ()
notOp = uOp f
  where f 0 = 1
        f _ = 0

dup :: Eval ()
dup = empty >>= f
  where f False = pop >>= push2
        f True = push 0
        push2 x = push x >> push x

randDir :: Eval EvalDir
randDir = randomR (0, 3) . stdGen <$> get >>= f
  where f :: (Int, StdGen) -> Eval EvalDir
        f (val, gen) = return ([minBound .. maxBound] !! val) <* modify (\x -> x { stdGen = gen })

changeMode :: EvalMode -> Eval ()
changeMode mode = modify $ \x -> x { evalMode = mode }

changeDir :: EvalDir -> Eval ()
changeDir d = modify $ \x -> x { evalDir = d }

swap :: Eval ()
swap = (,) <$> pop <*> pop >>= push2
  where push2 (x, y) = push x >> push y

pop_ :: Eval ()
pop_ = () <$ pop

putVal :: (Int, Int) -> Int -> Eval ()
putVal (x, y) v = modify $ \s -> s { evalSource = replace (x, y) v $ evalSource s }

putOp :: Eval ()
putOp = liftM3 (,,) pop pop pop >>= f
  where f (y, x, v) = putVal (y, x) v

locate :: (Int, Int) -> SourceMap -> Int -- Returnning the ascii value
locate (x, y) s = ord $ s !! x !! y

replace :: (Int, Int) -> Int -> SourceMap -> SourceMap
replace (x, y) v s = replaceNth x (replaceNth y (chr v) (s !! x)) s

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 v (x:xs) = v:xs
replaceNth n v (x:xs) = x : replaceNth (n-1) v xs

getVal :: (Int, Int) -> Eval Int
getVal (x, y) = locate (y, x) . evalSource <$> get

getOp :: Eval ()
getOp = flip (,) <$> pop <*> pop >>= getVal >>= push

move :: Eval ()
move = modify $ \s -> s { evalPos = go <$> evalPos <*> evalDir <*> evalSource $ s }
  where go :: (Int, Int) -> EvalDir -> SourceMap -> (Int, Int)
        go (x, y) d s = g s $ add (x, y) $ f d
        f L = (0, -1)
        f R = (0, 1)
        f U = (-1, 0)
        f D = (1, 0)
        add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
        g s = g2 s . g1 s
        g2 s (u, v) = let l = length s in ((u + l) `mod` l, v)
        g1 s (u, v) = let l = length $ s !! u in (u, (v + l) `mod` l)

condMoveL :: Eval ()
condMoveL = f <$> pop >>= changeDir
  where f 0 = R
        f _ = L

condMoveR :: Eval ()
condMoveR = f <$> pop >>= changeDir
  where f 0 = D
        f _ = U

toggleStringMode :: Eval ()
toggleStringMode = modify $ \s -> s { evalMode = f $ evalMode s }
  where f NormalMode = StringMode
        f StringMode = NormalMode

writeInt :: Eval ()
writeInt = show <$> pop >>= tell

writeChar :: Eval ()
writeChar = pure <$> chr <$> pop >>= tell

fetch :: Eval Char
fetch = chr . (locate <$> evalPos <*> evalSource) <$> get

data Op
  = Val Char
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Not
  | Comp
  | MoveR
  | MoveL
  | MoveU
  | MoveD
  | MoveRand
  | CondMoveL
  | CondMoveU
  | DQuote
  | Dup
  | Swap
  | Pop_
  | WriteInt
  | WriteChar
  | Hash
  | Put
  | Get
  | EOF
  | NOP
  deriving (Show, Eq, Ord)

fetchOp :: Eval Op
fetchOp = f <$> fetch
  where f :: Char -> Op
        f '+' = Add
        f '-' = Sub
        f '*' = Mul
        f '/' = Div
        f '%' = Mod
        f '!' = Not
        f '`' = Comp
        f '>' = MoveR
        f '<' = MoveL
        f '^' = MoveU
        f 'v' = MoveD
        f '?' = MoveRand
        f '_' = CondMoveL
        f '|' = CondMoveU
        f '"' = DQuote
        f ':' = Dup
        f '\\' = Swap
        f '$' = Pop_
        f '.' = WriteInt
        f ',' = WriteChar
        f '#' = Hash
        f 'p' = Put
        f 'g' = Get
        f '@' = EOF
        f ' ' = NOP
        f ch = Val ch

runOp :: Op -> Eval ()
runOp op = f $ M.lookup op opTable
  where opTable :: M.Map Op (Eval ())
        opTable = M.fromList
          [ (Add, addOp)
          , (Sub, subOp)
          , (Mul, mulOp)
          , (Div, divOp)
          , (Mod, modOp)
          , (Not, notOp)
          , (Comp, gtOp)
          , (MoveR, changeDir R)
          , (MoveL, changeDir L)
          , (MoveU, changeDir U)
          , (MoveD, changeDir D)
          , (MoveRand, randDir >>= changeDir)
          , (CondMoveL, condMoveL)
          , (CondMoveU, condMoveR)
          , (DQuote, toggleStringMode)
          , (Dup, dup)
          , (Swap, swap)
          , (Pop_, pop_)
          , (WriteInt, writeInt)
          , (WriteChar, writeChar)
          , (Hash, move)
          , (Put, putOp)
          , (Get, getOp)
          ]
        f :: Maybe (Eval ()) -> Eval ()
        f Nothing = return ()
        f (Just g) = g

interpret :: StdGen -> String -> String
interpret gen source = execEval (buildState gen source) eval

buildSourceMap :: String -> SourceMap
buildSourceMap = paddingMap . lines

paddingMap :: [String] -> [String]
paddingMap src = fmap f src
  where l = maximum $ length <$> src
        f s = s ++ replicate (l - length s) ' '

buildState :: StdGen -> String -> EvalState
buildState gen source = emptyState { stdGen = gen, evalSource = buildSourceMap source }

getMode :: Eval EvalMode
getMode = evalMode <$> get

eval :: Eval ()
eval = getMode >>= go
  where go StringMode = fetch >>= g >> move >> eval
        go NormalMode = fetchOp >>= f
          where f EOF = return ()
                f tok = run tok >> move >> eval
                run tok = case tok of
                            NOP -> return ()
                            Val ch -> push $ read [ch]
                            otherwise -> runOp tok
        g :: Char -> Eval ()
        g '"' = toggleStringMode
        g ch = push $ ord ch

eval1 :: Eval ()
eval1 = getMode >>= go
  where go StringMode = fetch >>= g >> move
        go NormalMode = fetchOp >>= f
          where f EOF = return ()
                f tok = run tok >> move
                run tok = trace ("Running operation " ++ show tok ++ "\n") $ case tok of
                                                                               NOP -> return ()
                                                                               Val ch -> push $ read [ch]
                                                                               otherwise -> runOp tok
        g :: Char -> Eval ()
        g '"' = toggleStringMode
        g ch = push $ ord ch

type InterpState = (EvalState, String)

showStack :: [Int] -> String
showStack [] = "Empty Stack"
showStack s = mconcat . fmap (++" ") . fmap show . reverse $ s

insertAt :: a -> Int -> [a] -> [a]
insertAt x 0 xs = x : xs
insertAt _ _ [] = []
insertAt v n (x:xs) = x : insertAt v (n-1) xs

showSourceMap :: (Int, Int) -> SourceMap -> String
showSourceMap (x, y) s = unlines $ insertCursor (x, y) s
  where insertCursor (x, y) = insertAt (replicate y '~' ++ "*") (x+1)

showSourceMap' :: (Int, Int) -> SourceMap -> String
showSourceMap' (x, y) s = unlines . replaceNth x (putCursor y line) . fmap marginLine $ s
  where line = marginLine $ s !! x

marginLine :: String -> String
marginLine = (' ':) . (>>= (:" "))

putCursor :: Int -> String -> String
putCursor x = replaceNth (2*x) '[' . replaceNth (2*x + 2) ']'

showState :: InterpState -> String
showState (EvalState { evalSource = src
                         , evalPos = pos
                         , evalStack = stack
                         , evalDir = d
                         , evalMode = mode }, output)
  = "-----\n" ++ ds ++ " " ++ modes ++ "\n" ++ srcs ++ "\nStack: " ++ stacks ++ "\nOutput: " ++ output
  where ds = show d
        modes = show mode
        srcs = showSourceMap' pos src
        stacks = showStack stack

evalSeq :: [Eval ()]
evalSeq = iterate (>> eval1) eval1

stateSeq :: EvalState -> [InterpState]
stateSeq s = (s, "") : fmap (runEval s) evalSeq

debugRun :: Int -> StdGen -> String -> IO ()
debugRun n gen source = mapM_ putStrLn . take n . fmap showState $ stateSeq (buildState gen source)

debugTrace :: FilePath -> Int -> StdGen -> String -> IO ()
debugTrace path n gen src = (>> putStrLn "Done!") . writeFile path . unlines . take n . fmap showState . stateSeq $ buildState gen src
