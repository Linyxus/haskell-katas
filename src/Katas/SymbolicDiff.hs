-- | Kata: Symbolic differentiation of prefix expressions - 2 kyu
{-# LANGUAGE TupleSections #-}
module Katas.SymbolicDiff where
import Control.Applicative
import Control.Monad
import Data.Bifunctor (first)
import Data.Char

import Debug.Trace (trace)

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

parse :: Parser a -> String -> Maybe a
parse m s = f . filter g $ runParser m s
  where f ((x, []):_) = Just x
        f [] = Nothing
        g (x, []) = True
        g _ = False

parseAll :: Parser a -> String -> [(a, String)]
parseAll = runParser

joinParser :: Parser (Parser a) -> Parser a
joinParser p = Parser $ runParser p >=> f
  where f (p', s') = runParser p' s'

instance Functor Parser where
  fmap f p = Parser $ fmap (first f) <$> runParser p

instance Applicative Parser where
  pure x = Parser $ pure . (x,)
  mf <*> mx = Parser $ runParser mf >=> f
    where f (g, s') = runParser (g <$> mx) s'

instance Monad Parser where
  return = pure
  mx >>= f = joinParser $ f <$> mx

instance Alternative Parser where
  empty = Parser $ const []
  p <|> q = Parser $ (++) <$> runParser p <*> runParser q

instance MonadPlus Parser

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = []
        f (x : xs) | p x = [(x, xs)]
                   | otherwise = []

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = foldr f $ pure []
  where f :: Char -> (Parser String -> Parser String)
        f = liftA2 (:) . char

choice :: [Parser a] -> Parser a
choice = msum

pSpace :: Parser Char
pSpace = char ' '

space :: Parser String
space = many pSpace

token :: Parser a -> Parser a
token = (<* space)

pDigit :: Parser Char
pDigit = satisfy isDigit

pNumber :: Parser Double
pNumber = read <$> p
  where p = liftA2 (++) (string "-" <|> pure []) $ some pDigit

symbol :: String -> Parser String
symbol = token . string

between :: String -> String -> Parser a -> Parser a
between l r p = string l >> p <* string r

data Op = Plus | Minus | Mult | Div | Pow deriving (Eq, Show)

data Func = Cos | Sin | Tan | Exp | Ln deriving (Eq, Show)

data AExp = AVar
          | AInt Double
          | AOp Op AExp AExp
          | AFunc Func AExp
          deriving (Eq)

pOp :: Parser Op
pOp = choice [ Plus <$ symbol "+"
             , Minus <$ symbol "-"
             , Mult <$ symbol "*"
             , Div <$ symbol "/"
             , Pow <$ symbol "^"
             ]

pFunc :: Parser Func
pFunc = choice [ Cos <$ symbol "cos"
               , Sin <$ symbol "sin"
               , Tan <$ symbol "tan"
               , Exp <$ symbol "exp"
               , Ln <$ symbol "ln"
               ]

pAVar :: Parser AExp
pAVar = AVar <$ symbol "x"

pAInt :: Parser AExp
pAInt = AInt <$> token pNumber

pAOp :: Parser AExp
pAOp = between "(" ")" $ AOp <$> pOp <*> pAExp <*> pAExp

pAFunc :: Parser AExp
pAFunc = between "(" ")" $ AFunc <$> pFunc <*> pAExp

pAExp :: Parser AExp
pAExp = choice [ pAVar, pAInt, token pAOp, token pAFunc ]

diffAExp :: AExp -> AExp
diffAExp (AInt _) = AInt 0
diffAExp AVar = AInt 1
diffAExp (AOp Plus a1 a2) = AOp Plus (diffAExp a1) (diffAExp a2)
diffAExp (AOp Minus a1 a2) = AOp Minus (diffAExp a1) (diffAExp a2)
diffAExp (AOp Mult a1 a2) = AOp Plus (AOp Mult (diffAExp a1) a2) (AOp Mult a1 (diffAExp a2))
diffAExp (AOp Div a1 a2) = AOp Div (AOp Minus (AOp Mult (diffAExp a1) a2) (AOp Mult a1 (diffAExp a2))) (AOp Pow a2 (AInt 2))
diffAExp (AOp Pow a1 (AInt 0)) = AInt 0
diffAExp (AOp Pow a1 (AInt n)) = AOp Mult (AInt n) (AOp Pow a1 (AInt $ n-1))
diffAExp (AOp Pow a1 a2) = AOp Mult (AOp Pow a1 a2)
                                    (AOp Plus
                                         (AOp Mult (diffAExp a2) (AFunc Ln a1))
                                         (AOp Mult a2 (AOp Div (diffAExp a1) a1)))
diffAExp (AFunc Cos a) = chain a $ AOp Mult (AInt (-1)) (AFunc Sin a)
diffAExp (AFunc Sin a) = chain a $ AFunc Cos a
diffAExp (AFunc Tan a) = chain a $ AOp Plus (AInt 1) (AOp Pow (AFunc Tan a) (AInt 2))
diffAExp e@(AFunc Exp a) = chain a e
diffAExp (AFunc Ln a) = chain a $ AOp Div (AInt 1) a

chain :: AExp -> AExp -> AExp
chain a = AOp Mult (diffAExp a)

showOp :: Op -> String
showOp Plus = "+"
showOp Minus = "-"
showOp Mult = "*"
showOp Div = "/"
showOp Pow = "^"

showFunc :: Func -> String
showFunc Cos = "cos"
showFunc Sin = "sin"
showFunc Tan = "tan"
showFunc Exp = "exp"
showFunc Ln = "ln"

showAExp :: AExp -> String
showAExp (AInt x) = showDouble x
showAExp AVar = "x"
showAExp (AOp op a1 a2) = "(" ++ showOp op ++ " " ++ showAExp a1 ++ " "
                          ++ showAExp a2 ++ ")"
showAExp (AFunc f a) = "(" ++ showFunc f ++ " " ++ showAExp a ++ ")"

canBeInt :: Double -> Bool
canBeInt x = fromIntegral (truncate x) == x

showDouble :: Double -> String
showDouble x | canBeInt x = show $ truncate x
             | otherwise = show x

instance Show AExp where show = showAExp

-- >>> s = "(sin (* 2 x))"
-- >>> Just a = parse pAExp s
-- >>> a
-- (sin (* 2 x))
-- >>> diffAExp a
-- (* (+ (* 0 x) (* 2 1)) (cos (* 2 x)))

calcOp :: Op -> Double -> Double -> Double
calcOp Plus = (+)
calcOp Minus = (-)
calcOp Mult = (*)
calcOp Div = (/)
calcOp Pow = (**)

shallowSimpl :: AExp -> AExp
shallowSimpl (AOp Mult (AInt 0) a) = AInt 0
shallowSimpl (AOp Mult (AInt 1) a) = a
shallowSimpl (AOp Mult a (AInt 0)) = AInt 0
shallowSimpl (AOp Mult a (AInt 1)) = a
shallowSimpl (AOp Plus (AInt 0) a) = a
shallowSimpl (AOp Plus a (AInt 0)) = a
shallowSimpl (AOp op (AInt x) (AInt y)) = AInt $ calcOp op x y
shallowSimpl (AOp Pow a (AInt 0)) = AInt 1
shallowSimpl (AOp Pow a (AInt 1)) = a
shallowSimpl x = x

simpl :: AExp -> AExp
simpl (AOp op a1 a2) = shallowSimpl (AOp op (simpl a1) (simpl a2))
simpl (AFunc op a) = shallowSimpl (AFunc op (simpl a))
simpl x = x

parse' :: String -> AExp
parse' = f . parse pAExp
  where f (Just x) = x

diff :: String -> String
diff s = trace s $ show . simpl . diffAExp . parse' $ s

-- >>> s = "(- (+ x x) x)"
-- >>> diff s
-- "(- (+ x x) x)
-- 1"
-- >>> diff "(* 3 (^ x 2))"
-- "(* 3 (^ x 2))
-- (* 3 (* 2 x))"
