{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Katas.PolyvariadicFunctions where

class AddRes r where
  polyAdd :: Int -> r

instance AddRes Int where
  polyAdd = id

instance (a ~ Int, AddRes r) => AddRes (a -> r) where
  polyAdd x = polyAdd . (+x)

class ConsRes r where
  polyList :: Int -> r

instance ConsRes [Int] where
  polyList = pure

instance (a ~ Int, ConsRes r) => ConsRes (a -> r) where
  polyList = polyList
