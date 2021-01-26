{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}
module Katas.SimpleSQLEngine where

import           Control.Applicative
import           Control.Monad       ((>=>))
import           Data.Bifunctor      (first, second)
import           Data.Char           (isAlphaNum, isDigit, isSpace, toLower)
import           Data.Foldable       (asum)
import qualified Data.Map            as M

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

-- data types

data Query = Query { selectClause :: Select
                   , fromClause   :: String
                   , joinClause   :: [Join]
                   , whereClause  :: Maybe ValCond
                   }
  deriving (Eq, Show)

newtype Select = Select [ColId]
  deriving (Eq, Show)

data Join = Join { joinTable :: String
                 , joinCond  :: ValCond
                 }
  deriving (Eq, Show)

data ColId = ColId { tableName :: String
                   , colName   :: String
                   }
  deriving (Eq, Show, Ord)

data ValCond = ValCond Val Op Val
  deriving (Eq, Show)

data Val = ColVal ColId
         | ConstNum Int
         | ConstStr String
         deriving (Eq, Show)

data Op = Eq | Gt | Lt | Ge | Le | Neq
  deriving (Eq, Show)

-- query parser

pQuery :: Parser Query
pQuery = Query <$> sc pSelect
               <*> sc pFrom
               <*> many (sc pJoin)
               <*> optional (sc pWhere)

-- >>> parse pQuery "select film.name, director.name from director join movie on director.id = movie.directorId"
-- Just (Query {selectClause = Select [ColId {tableName = "film", colName = "name"},ColId {tableName = "director", colName = "name"}], fromClause = "director", joinClause = [Join {joinTable = "movie", joinCond = ValCond (ColVal (ColId {tableName = "director", colName = "id"})) Eq (ColVal (ColId {tableName = "movie", colName = "directorId"}))}], whereClause = Nothing})

pSelect :: Parser Select
pSelect = fmap Select $ stringCI "select" >> sc pColId `sepBy1` ','

pFrom :: Parser String
pFrom = stringCI "from" >> sc symbolP

pJoin :: Parser Join
pJoin = stringCI "join" >> Join <$> sc symbolP <*> sc (stringCI "on" >> sc pValCond)

pWhere :: Parser ValCond
pWhere = stringCI "where" >> sc pValCond

-- >>> parse pSelect "select filer.actorId, director.filmId"
-- Just (Select [ColId {tableName = "filer", colName = "actorId"},ColId {tableName = "director", colName = "filmId"}])

-- >>> parse pSelect "SeleCt filer.actorId, director.filmId"
-- Just (Select [ColId {tableName = "filer", colName = "actorId"},ColId {tableName = "director", colName = "filmId"}])

-- >>> parse pJoin "join director on film.directorId == director.id"

pColId :: Parser ColId
pColId = ColId <$> (symbolP <* char '.') <*> symbolP

-- >>> parse pColId "film.actorId"
-- Just (ColId {tableName = "film", colName = "actorId"})

pValCond :: Parser ValCond
pValCond = ValCond <$> pVal <*> sc pOp <*> sc pVal

-- >>> parse pValCond "film.directorId = 'name'''"
-- Just (ValCond (ColVal (ColId {tableName = "film", colName = "directorId"})) Eq (ConstStr "name'"))

pOp :: Parser Op
pOp = choice [ Eq <$ string "="
             , Ge <$ string ">="
             , Le <$ string "<="
             , Gt <$ string ">"
             , Lt <$ string "<"
             , Neq <$ string "<>"
             ]

pVal :: Parser Val
pVal = choice [ ColVal <$> pColId
              , ConstNum <$> integerP
              , ConstStr <$> stringP
              ]

symCharP :: Parser Char
symCharP = satisfy f
  where f x = isAlphaNum x || x == '_'

symbolP :: Parser String
symbolP = some symCharP

-- query executor

type Schema = [String]
type Tuple = M.Map ColId String

type Database = M.Map String [Tuple]

expect :: Maybe a -> a
expect (Just x) = x

createDatabase :: [(String, [[(String, String)]])] -> Database
createDatabase = M.fromList . fmap f
  where f :: (String, [[(String, String)]]) -> (String, [Tuple])
        f (tn, xs) = (tn, createTuple tn <$> xs)

createTuple :: String -> [(String, String)] -> Tuple
createTuple tn = M.fromList . fmap (first f)
  where f :: String -> ColId
        f = ColId tn

executeFrom :: String -> Database -> [Tuple]
executeFrom t db = expect $ M.lookup t db

executeJoin :: Join -> Database -> [Tuple] -> [Tuple]
executeJoin = undefined
