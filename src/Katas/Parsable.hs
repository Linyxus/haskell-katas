module Katas.Parsable where
import           Data.Char (isDigit)

parses :: String -> Bool
parses [] = False
parses "-" = False
parses ('-':xs) = parses' xs
parses xs = parses' xs

parses' :: String -> Bool
parses' [] = True
parses' (x:xs) | isDigit x = parses' xs
               | otherwise = False

