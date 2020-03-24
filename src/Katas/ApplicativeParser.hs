{-# LANGUAGE TupleSections #-}
module Katas.ApplicativeParser where

import Data.Char
import Data.Bifunctor (first, second)
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P p) = P $ map g . p
  where g (s, x) = (s, f x)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P f
  where f [] = []
        f (x:xs)
          | p x = [(xs, x)]
          | otherwise = []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ pure . (,x)

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(P pf) <@> (P px) = P $ (>>=go) . pf
  where go (s, f) = second f <$> px s

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = pmap const pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = pmap (const id) pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP = foldr (\ x -> (<@>) ((:) <#> charP x)) (inject [])

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) (P p1) (P p2) = P $ (++) <$> p1 <*> p2

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = inject [] <<>> some p

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser (P p) cs = map snd . filter ((=="") . fst) $ p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = f $ runParser p cs
  where f [x] = Just x
        f _ = Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr (BinOpE op e1 e2) = runOp op (evalExpr e1) (evalExpr e2)
evalExpr (NegE e1) = (\x -> -x) $ evalExpr e1
evalExpr ZeroE = 0

runOp :: BinOp -> Int -> Int -> Int
runOp AddBO = (+)
runOp MulBO = (*)

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
--
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique pExpr

pExpr :: Parser Expr
pExpr = pConst <<>> pBinOpExpr <<>> pNeg <<>> pZero

pConst :: Parser Expr
pConst = ConstE . read <#> some pDigit

pBinOpExpr :: Parser Expr
pBinOpExpr = between '(' ')' $ f <#> pExpr <@> pBinOp <@> pExpr
  where f e1 op e2 = BinOpE op e1 e2

pBinOp :: Parser BinOp
pBinOp = AddBO <# stringP " + " <<>> MulBO <# stringP " * "

pNeg :: Parser Expr
pNeg = NegE <#> (charP '-' @> pExpr)

pZero :: Parser Expr
pZero = ZeroE <# charP 'z'

pDigit :: Parser Char
pDigit = predP (`elem` ['0'..'9'])

between :: Char -> Char -> Parser a -> Parser a
between l r p = charP l @> p <@ charP r
