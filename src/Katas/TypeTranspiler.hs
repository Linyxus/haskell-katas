-- | Kata: Type Transpiler - 3 kyu
{-# LANGUAGE FlexibleContexts #-}
module Katas.TypeTranspiler where
import Control.Applicative
import Data.Char
import Debug.Trace

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe a
parse m s = runParser m s >>= f
  where f (val, []) = return val
        f _ = Nothing

joinParser :: Parser (Parser a) -> Parser a
joinParser p = Parser $ \s -> runParser p s >>= f
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

between :: Parser String -> Parser String -> Parser a -> Parser a
between l r p = l >> p <* r

between' s = between s s

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

alphas = ['a' .. 'z'] ++ ['A' .. 'Z']
digits = ['0' .. '9']

alphaP :: Parser Char
alphaP = satisfy (`elem` alphas)

digitP :: Parser Char
digitP = satisfy (`elem` digits)

underscoreP :: Parser Char
underscoreP = char '_'

data Type
  = FuncT [Type] Type
  | UserT [UserType]
  deriving (Eq, Show)

data UserType = UserType String [TParam]
  deriving (Eq, Show)

data TParam = Arb | In Type | Out Type | Type Type
  deriving (Eq, Show)

pType :: Parser Type
pType = choice
  [ pUserT
  , pFuncT
  , between (symbol "(") (symbol ")") pType
  ]

pName :: Parser String
pName = token $ (:) <$> alphaOrUs <*> many p
  where alphaOrUs = alphaP <|> underscoreP
        p = alphaOrUs <|> digitP

pFuncT :: Parser Type
pFuncT = do
  params <- between (symbol "(") (symbol ")") pParams
  symbol "->"
  t <- pType
  return $ FuncT params t

pParams :: Parser [Type]
pParams = pType `sepBy` symbol ","

pUserT :: Parser Type
pUserT = UserT <$> pSimpleUserT `sepBy1` symbol "."

pSimpleUserT :: Parser UserType
pSimpleUserT = do
  name <- pName
  tp <- f <$> optional (between (symbol "<") (symbol ">") pTParams)
  return $ UserType name tp
  where f :: Maybe [TParam] -> [TParam]
        f Nothing = []
        f (Just xs) = xs

pTParams = pTParam `sepBy1` symbol ","

pTParam :: Parser TParam
pTParam = choice [pArb, pIn, pOut, Type <$> pType]

pArb :: Parser TParam
pArb = Arb <$ symbol "*"

pIn :: Parser TParam
pIn = symbol "in" >> In <$> pType

pOut :: Parser TParam
pOut = symbol "out" >> Out <$> pType

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep >> p)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

translate :: Type -> String
translate (UserT ut) = [transUserType t | t <- ut] `unwordsBy` "."
translate (FuncT p r) = transFunc p r

transUserType :: UserType -> String
transUserType (UserType name tparams) = rename name ++ transTParams tparams

rename :: String -> String
rename "Int" = "Integer"
rename "Unit" = "Void"
rename x = x

transTParam :: TParam -> String
transTParam Arb = "?"
transTParam (Out t) = "? extends " ++ translate t
transTParam (In t) = "? super " ++ translate t
transTParam (Type t) = translate t

transTParams :: [TParam] -> String
transTParams [] = ""
transTParams xs = (++">") . ("<"++) $ [transTParam x | x <- xs] `unwordsBy` ","

unwordsBy :: [String] -> String -> String
unwordsBy [] _ = []
unwordsBy (x:xs) sep = x ++ concat [sep ++ x | x <- xs]

transFunc :: [Type] -> Type -> String
transFunc p r = funcSymb (length p) ++ transParams (p ++ [r])

funcSymb :: Int -> String
funcSymb n = "Function" ++ show n

transParams :: [Type] -> String
transParams xs = "<" ++ [translate x | x <- xs] `unwordsBy` "," ++ ">"

run :: String -> Either String String
run src = trace (show src) $ (translate <$>) . f $ parse ( sc >> pType ) src
  where f :: Maybe Type -> Either String Type
        f Nothing = Left "Hugh?"
        f (Just x) = Right x

transpile :: String -> Either String String
transpile = run
