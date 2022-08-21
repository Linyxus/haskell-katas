{-# LANGUAGE TupleSections #-}
module Katas.EvalMathExpr where

import Data.List (isPrefixOf)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Data.Char (isDigit, isSpace)

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

parseAll :: Parser a -> String -> [(a, String)]
parseAll (Parser p) = p

parseOnly :: Parser a -> String -> Maybe (a, String)
parseOnly = (g <$>) . parseAll
  where g [] = Nothing
        g (x:xs) = Just x

instance Functor Parser where
  fmap f (Parser pa) = Parser $ (g <$>) <$> pa
    where g (x, s) = (f x, s)

instance Applicative Parser where
  pure x = Parser $ pure . (x,)
  Parser pf <*> pa = Parser $ (>>= g) <$> pf
    where
      g (f, s) =
        case f <$> pa of
          Parser pb -> pb s

instance Alternative Parser where
  empty = Parser $ const []
  Parser pa <|> Parser pb = Parser $ \s -> pa s ++ pb s

flattenParser :: Parser (Parser a) -> Parser a
flattenParser (Parser ppa) = Parser $ (>>= g) <$> ppa
  where
    g (Parser pa, s) = pa s

instance Monad Parser where
  return = pure
  pa >>= fapb = flattenParser $ fapb <$> pa

eofP :: Parser ()
eofP = Parser $ f
  where f [] = [((), "")]
        f _ = []

charPredP :: (Char -> Bool) -> Parser Char
charPredP pred = Parser $ g
  where
    g [] = []
    g (x:xs) | pred x = pure $ (x, xs)
             | otherwise = []

-- >>> parseOnly (charP 'a') "abc"
-- Just ('a',"bc")
-- >>> parseOnly (charP 'b') "abc"
-- Nothing

charP :: Char -> Parser Char
charP = charPredP . (==)

-- >>> parseOnly (stringP "hello") "hello world"
-- Just ("hello"," world")
-- >>> parseOnly (stringP "") "hello world"
-- Just ("","hello world")
-- >>> parseOnly (stringP "world") "hello world"
-- Nothing
-- >>> parseOnly (some $ stringP "hello") "hellohellohello world"
-- Just (["hello","hello","hello"]," world")
-- >>> parseOnly (some $ stringP "hello") "world"
-- Nothing
-- >>> parseOnly (many $ stringP "hello") "world"
-- Just ([],"world")

stringP :: String -> Parser String
stringP = Parser . recur id
  where
    recur cont [] xs = pure $ (cont [], xs)
    recur cont (x:xs) (y:ys) | x == y = recur (cont . (x:)) xs ys
                             | otherwise = []
    recur _ xs [] = []

spaceP :: Parser Char
spaceP = charPredP isSpace

spacesP :: Parser String
spacesP = many spaceP

spaced :: Parser a -> Parser a
spaced = (<* spacesP)

-- >>> parseOnly (many . spaced $ stringP "hello") "hello hello   hellohello world!"
-- Just (["hello","hello","hello","hello"],"world!")

optional :: Parser a -> Parser (Maybe a)
optional pa = Just <$> pa <|> pure Nothing

-- >>> parseOnly (choices [stringP "hello", stringP "world"]) "hello, world"
-- Just ("hello",", world")
-- >>> parseOnly (choices [stringP "hello", stringP "world"]) "world, hello"
-- Just ("world",", hello")

choices :: [Parser a] -> Parser a
choices = recur empty
  where recur acc [] = acc
        recur acc (x:xs) = recur (acc <|> x) xs

digitP :: Parser Char
digitP = charPredP isDigit

digitsP :: Parser String
digitsP = some digitP

-- >>> parseOnly digitsP "12345a"
-- Just ("12345","a")
-- >>> parseOnly natP "12345a"
-- Just (12345,"a")

natP :: Parser Int
natP = read <$> digitsP

doubleP :: Parser Double
doubleP = natP >>= (<$> floatP) . f
  where
    getFloat :: String -> Double
    getFloat s = read $ "0." ++ s

    floatP :: Parser (Maybe Double)
    floatP = optional . fmap getFloat $ stringP "." >> digitsP

    f :: Int -> (Maybe Double) -> Double
    f n (Just x) = fromIntegral n + x
    f n Nothing = fromIntegral n

-- >>> parseOnly doubleP "3.1415"
-- Just (3.1415,"")
-- >>> parseOnly doubleP "3"
-- Just (3.0,"")

-- Grammar rules
-- term ::= number | '(' expr ')'
-- unary ::= '-'term | term
-- factor ::= unary | unary '*' unary | unary '/' unary
-- expr ::= factor | factor '+' factor | factor '-' factor

data Expr
  = Num Double
  | Neg Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show)

-- >>> parseOnly exprP "1.23"
-- Just (Num 1.23,"")
-- >>> parseOnly exprP "1 + -1"
-- Just (Add (Num 1.0) (Neg (Num 1.0)),"")
-- >>> parseOnly exprP "(2 / (2 + 3.33) * 4) - -6"
-- Just (Sub (Mul (Div (Num 2.0) (Add (Num 2.0) (Num 3.33))) (Num 4.0)) (Neg (Num 6.0)),"")
-- >>> parseOnly exprP "(2 / (2 + 3.33) * 4) - ---6"
-- Just (Sub (Mul (Div (Num 2.0) (Add (Num 2.0) (Num 3.33))) (Num 4.0)) (Neg (Neg (Neg (Num 6.0)))),"")

exprP :: Parser Expr
exprP = exprP'

numberP :: Parser Expr
numberP = Num <$> spaced doubleP

termP :: Parser Expr
termP = numberP <|> (spaced (charP '(') >> exprP <* spaced (charP ')'))

unaryP :: Parser Expr
unaryP = p1 <|> termP
  where p1 = fmap Neg $ charP '-' >> unaryP

mkBinParser :: [Parser a] -> (a -> Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
mkBinParser opPs builder atomP = atomP >>= (<$> atomsP) . f
  where
    f x xs = foldl g x xs
    g x (op, y) = builder op x y
    opP = choices opPs
    atomsP = many $ (,) <$> opP <*> atomP

factorP :: Parser Expr
factorP = mkBinParser [spaced $ charP '*', spaced $ charP '/'] f unaryP
  where f '/' x y = Div x y
        f '*' x y = Mul x y
        f op _ _ = error $ "Unknown op: " ++ show op

exprP' :: Parser Expr
exprP' = mkBinParser [spaced $ charP '+', spaced $ charP '-'] f factorP
  where f '+' x y = Add x y
        f '-' x y = Sub x y
        f op _ _ = error $ "Unknown op: " ++ show op

eval :: Expr -> Double
eval (Num x) = x
eval (Neg e) = -(eval e)
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2

parseExpr_ :: String -> Expr
parseExpr_ s = case parseOnly (spacesP >> exprP <* eofP) s of
                 Nothing -> error $ "Can not parse " ++ show s
                 Just (e, _) -> e

calc :: String -> Double
calc = eval . parseExpr_

-- >>> calc "(2 / (2 + 3.33) * 4) - -6"
-- 7.50093808630394
-- >>> calc "2 / (2 + 3) * 4.33 - -6"
-- 7.732

