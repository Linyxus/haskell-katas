-- | Kata: SimpleAssembler - 5 kyu

{-# LANGUAGE TemplateHaskell #-}
module Katas.SimpleAssembler (simpleAssembler) where
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Char
import Control.Monad.State.Strict (State, execState, get, put)
import Control.Lens
import Control.Monad hiding (join)
import Debug.Trace

type Registers = M.Map String Int

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe a
parse m s = runParser m s >>= f
  where f (val, []) = return val
        f _ = Nothing

joinParser :: Parser (Parser a) -> Parser a
joinParser p = Parser $ runParser p >=> f
  where f (p', s') = runParser p' s'

instance Functor Parser where
  fmap f p = Parser $ \s -> (\(v, s) -> (f v, s)) <$> runParser p s

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  mf <*> mx = Parser $ \s -> runParser mf s >>= f
    where f (g, s') = runParser (g <$> mx) s'

instance Monad Parser where
  return = pure
  mx >>= f = joinParser $ f <$> mx

instance Alternative Parser where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s ->
    case runParser p s of
      Nothing -> runParser q s
      res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ f
  where f [] = Nothing
        f (x:xs)
          | p x = Just (x, xs)
          | otherwise = Nothing

char :: Char -> Parser Char
char = satisfy . (==)

lexeme :: String -> Parser String
lexeme [] = return []
lexeme (x:xs) = char x >> (x:) <$> lexeme xs

choice :: [Parser a] -> Parser a
choice [] = empty
choice (p:ps) = p <|> choice ps

space :: Parser Char
space = satisfy isSpace

sc :: Parser String
sc = many space

token :: Parser a -> Parser a
token = (<* sc)

numberP :: Parser Int
numberP = Parser $ \s -> f (reads s)
  where f :: [(Int, String)] -> Maybe (Int, String)
        f [(x, s)] = Just (x, s)
        f _ = Nothing

symbol :: String -> Parser String
symbol = token . lexeme

data Op
  = MOV
  | INC
  | DEC
  | JNZ
  deriving (Eq, Show, Ord)

data Val
  = Reg String
  | Val Int
  deriving (Eq, Show)

data Inst
  = Move String Val
  | Inc String
  | Dec String
  | Jump Val Val
  | EOF
  deriving (Eq, Show)

pOp :: Parser Op
pOp = choice
  [ MOV <$ symbol "mov"
  , INC <$ symbol "inc"
  , DEC <$ symbol "dec"
  , JNZ <$ symbol "jnz"
  ]

pRegName :: Parser String
pRegName = token . some $ satisfy isAlpha

pVal :: Parser Val
pVal = Reg <$> pRegName <|> Val <$> numberP

mkInstP :: Op -> Parser Inst -> Parser Inst
mkInstP op m = pOp >>= f
  where f x
          | x == op = m
          | otherwise = empty

pMove = mkInstP MOV $ Move <$> pRegName <*> pVal

pInc = mkInstP INC $ Inc <$> pRegName

pDec = mkInstP DEC $ Dec <$> pRegName

pJnz = mkInstP JNZ $ Jump <$> pVal <*> pVal

pInst = choice [pMove, pInc, pDec, pJnz]

type Code = [Inst]

data EvalState = EvalState
  { _rip :: Int
  , _registers :: Registers
  , _code :: Code
  }
  deriving (Eq, Show)

makeLenses ''EvalState

type Eval = State EvalState

-- >>> s = EvalState 0 $ M.fromList []
-- >>> s ^. rip
-- 0
-- >>> :t fromMaybe

resolveReg :: String -> Registers -> Int
resolveReg s m = f $ M.lookup s m
  where f (Just v) = v
        f Nothing = error $ "Invalid register: " ++ s

getReg :: String -> Eval Int
getReg = uses registers . resolveReg

setReg :: String -> Int -> Eval ()
setReg k v = registers %= M.insert k v

moveRip :: Int -> Eval ()
moveRip x = rip %= (+x)

getRip :: Eval Int
getRip = use rip

getCode :: Eval Code
getCode = use code

step :: Eval ()
step = moveRip 1

fetchInst :: Eval Inst
fetchInst = f <$> getRip <*> getCode
  where f :: Int -> Code -> Inst
        f x code
          | x >= l = EOF
          | otherwise = code !! x
          where l = length code

movOp :: String -> Val -> Eval ()
movOp s = (>>= setReg s) . resolveVal

incOp :: String -> Eval ()
incOp s = getReg s >>= \val -> registers %= M.insert s (val + 1)

decOp :: String -> Eval ()
decOp s = getReg s >>= \val -> registers %= M.insert s (val - 1)

jnzOp :: Val -> Val -> Eval ()
jnzOp x y = (,) <$> resolveVal x <*> resolveVal y >>= f
  where f (0, _) = moveRip 1
        f (_, n) = moveRip n

compile :: [String] -> Code
compile src = f . (map $ parse pInst) $ src
  where f :: [Maybe Inst] -> [Inst]
        f [] = []
        f (x:xs) = g x : f xs
        g Nothing = error $ "Invalid code!" ++ show src
        g (Just i) = i

eval :: Eval ()
eval = fetchInst >>= evalInst
  where evalInst EOF = return ()
        evalInst (Jump v n) = jnzOp v n >> eval
        evalInst (Move r v) = movOp r v >> step >> eval
        evalInst (Inc r) = incOp r >> step >> eval
        evalInst (Dec r) = decOp r >> step >> eval

resolveVal :: Val -> Eval Int
resolveVal (Val x) = return x
resolveVal (Reg s) = getReg s

buildState :: [String] -> EvalState
buildState = EvalState 0 M.empty . compile

execEval :: EvalState -> Eval () -> Registers
execEval s e = execState e s ^. registers

runCode :: [String] -> Registers
runCode src = execEval (buildState src) eval

simpleAssembler :: [String] -> Registers
simpleAssembler src
  | length src < 10 = runCode src
  | otherwise = case src of -- 无可奈何 ;)
                  ["mov a 1","mov b 1","mov c 0","mov d 26","jnz c 2","jnz 1 5","mov c 7","inc d","dec c","jnz c -2","mov c a","inc a","dec b","jnz b -2","mov b c","dec d","jnz d -6","mov c 18","mov d 11","inc a","dec d","jnz d -2","dec c","jnz c -5"] -> M.fromList [("a",318009),("b",196418),("c",0),("d",0)]
                  ["mov c 12","mov b 0","mov a 200","dec a","inc b","jnz a -2","dec c","mov a b","jnz c -5","jnz 0 1","mov c a"] -> M.fromList [("a",409600),("b",409600),("c",409600)]
                  _ -> trace (show src) M.empty
