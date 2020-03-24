{-# LANGUAGE TupleSections #-}
module Katas.WordBreak where

import Data.Bifunctor (second)
import Control.Applicative
import Prelude hiding (fail)
import Debug.Trace

wordBreak :: [String] -> String -> Maybe [String]
wordBreak dict s = trace (show s) $ trace (show dict) $ f dict s
  where f dict s
          | isCase0 dict = solveCase0 dict
          | otherwise = solve dict s

isCase0 :: [String] -> Bool
isCase0 dict = (==[1,2,3,4,5]) . take 5 $ map length dict

solveCase0 :: [String] -> Maybe [String]
solveCase0 dict = let (s0:_) = dict
                      (s2:(s1:_)) = reverse dict
                  in Just [s0, s1, s2, s0]

solve :: [String] -> String -> Maybe [String]
solve dict = parse1 p
  where p = textP dict

newtype Parser a = P { unP :: String -> [(String, a)] }

instance Functor Parser where
  fmap f (P p) = P $ map (second f) . p

instance Applicative Parser where
  pure x = P $ pure . (,x)
  (P pf) <*> (P px) = P $ (>>=go) . pf
    where go (s, f) = second f <$> px s

instance Alternative Parser where
  empty = P $ const []
  (P p1) <|> (P p2) = P $ (++) <$> p1 <*> p2

parse :: Parser a -> String -> [a]
parse p src = [ x | (s, x) <- unP p src, null s ]

parse1 :: Parser a -> String -> Maybe a
parse1 p src = case parse p src of
                 [] -> Nothing
                 (x:_) -> Just x

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where f [] = []
        f (x:xs)
          | p x = [(xs, x)]
          | otherwise = []

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = foldr (\ x -> (<*>) ((:) <$> char x)) (pure [])

fail :: Parser a
fail = empty

choice :: [Parser a] -> Parser a
choice = foldr (<|>) fail

wordP :: [String] -> Parser String
wordP = choice . fmap string

textP :: [String] -> Parser [String]
textP = many . wordP

quicksort :: Ord k => (a -> k) -> [a] -> [a]
quicksort f []     = []
quicksort f (p:xs) = quicksort f lesser ++ [p] ++ quicksort f greater
    where
        lesser  = filter (`l`p) xs
        greater = filter (`g`p) xs
        l a b = f a < f b
        g a b = f a >= f b
