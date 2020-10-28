{-# LANGUAGE DataKinds #-}
module Katas.Calculator where
import           Control.Applicative
import           Control.Monad       ((>=>))
import           Data.Bifunctor      (first, second)
import           Data.Char           (isAlphaNum, isDigit, isSpace, toLower)
import           Data.Foldable       (asum)
import           Data.Monoid         ((<>))
import           Debug.Trace         (trace)

evaluate :: String -> Double
evaluate s = evalExpr . parseExpr $ trace s s

-- parser

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

parseAll' :: Parser a -> String -> [a]
parseAll' p = fmap fst . runParser p

parseAll :: Parser a -> String -> [a]
parseAll p = fmap fst . filter f . runParser p
  where f (x, "") = True
        f _       = False

parse' :: Parser a -> String -> Maybe a
parse' p = f . runParser p
  where f []         = Nothing
        f ((x, _):_) = Just x

parse :: Parser a -> String -> Maybe a
parse p = f . parseAll p
  where f []    = Nothing
        f (x:_) = Just x

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) <$> p

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x, s)
  (Parser fg) <*> pa = Parser $ fg >=> go
    where go (g, s') = runParser (g <$> pa) s'

joinParser :: Parser (Parser a) -> Parser a
joinParser (Parser ppa) = Parser $ ppa >=> f
  where f (pa, s') = runParser pa s'

instance Monad Parser where
  return = pure
  ma >>= f = joinParser $ f <$> ma

instance Alternative Parser where
  empty = Parser $ const []
  (Parser pa) <|> (Parser pb) = Parser $ \s -> pa s <> pb s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f (x:s') | p x = pure (x, s')
                 | otherwise = []
        f _ = []

char :: Char -> Parser Char
char = satisfy . (==)

charCI :: Char -> Parser Char
charCI ch = satisfy f
  where f x = toLower x == toLower ch

string :: String -> Parser String
string = mapM char

stringCI :: String -> Parser String
stringCI = mapM charCI

choice :: [Parser a] -> Parser a
choice = asum

optionalS :: Parser String -> Parser String
optionalS p = p <|> pure ""

spaceP :: Parser Char
spaceP = satisfy isSpace

space :: Parser String
space = many spaceP

space1 :: Parser String
space1 = some spaceP

sc :: Parser a -> Parser a
sc = (space >>)

sc1 :: Parser a -> Parser a
sc1 = (space1 >>)

sepBy1 :: Parser a -> Char -> Parser [a]
sepBy1 p s = (:) <$> p <*> many (char s >> p)

sepBy :: Parser a -> Char -> Parser [a]
sepBy p s = sepBy1 p s <|> pure []

digitP :: Parser Char
digitP = satisfy isDigit

integerP :: Parser Int
integerP = f <$> p
  where f = read
        p :: Parser String
        p = (<>) <$> optionalS (string "-") <*> some digitP

escapeQuote :: Parser Char
escapeQuote = string "''" >> pure '\''

nonQuote :: Parser Char
nonQuote = satisfy (/= '\'')

stringP :: Parser String
stringP = string "'" >> many (escapeQuote <|> nonQuote) <* string "'"

-- datatypes

data Expr = Number Double
          | Add Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          deriving (Eq, Show)

-- parser for the arithmetic expression

-- expr ::= term ( ( "+" | "-" ) term )*
-- term ::= primary ( ( "*" | "/" ) primary )*
-- primary ::= number | "(" expr ")"

pExpr :: Parser Expr
pExpr = pTerm >>= go
  where go expr = choice [ Add expr <$> (pTerm `prefixBy` addP) >>= go
                         , Minus expr <$> (pTerm `prefixBy` minusP) >>= go
                         , pure expr
                         ]
        addP = sc $ char '+'
        minusP = sc $ char '-'

pFloat :: Parser Double
pFloat = do
  x <- integerP
  char '.'
  y <- integerP
  pure . read $ show x ++ "." ++ show y

pNumber :: Parser Expr
pNumber = sc . fmap Number $ f <$> integerP <|> pFloat
  where f = fromIntegral

pPrimary :: Parser Expr
pPrimary = pNumber
       <|> (sc (char '(') >> pExpr <* sc (char ')'))

pTerm :: Parser Expr
pTerm = pPrimary >>= go
  where go expr = choice [ Mult expr <$> (pPrimary `prefixBy` multP) >>= go
                         , Div expr <$> (pPrimary `prefixBy` divP) >>= go
                         , pure expr
                         ]
        multP = sc $ char '*'
        divP = sc $ char '/'

prefixBy :: Parser a -> Parser b -> Parser a
x `prefixBy` y = y >> x

parseExpr :: String -> Expr
parseExpr = f . parse pExpr
  where f (Just x) = x

evalExpr :: Expr -> Double
evalExpr (Number x)    = x
evalExpr (Add e1 e2)   = runOp (+) e1 e2
evalExpr (Minus e1 e2) = runOp (-) e1 e2
evalExpr (Mult e1 e2)  = runOp (*) e1 e2
evalExpr (Div e1 e2)   = runOp (/) e1 e2

runOp :: (Double -> Double -> Double) -> Expr -> Expr -> Double
runOp op e1 e2 = evalExpr e1 `op` evalExpr e2
