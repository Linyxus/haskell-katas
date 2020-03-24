-- | Kata: Tree By Levels - 4 kyu
module Katas.TreeByLevels where

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels x = printTree [x]

expand :: [Maybe (TreeNode a)] -> [Maybe (TreeNode a)]
expand = (>>= f)
  where f Nothing = []
        f (Just (TreeNode l r _)) = [l, r]

printTree :: [Maybe (TreeNode a)] -> [a]
printTree [] = []
printTree xs = f xs ++ printTree (expand xs)
  where f [] = []
        f (Nothing:xs) = f xs
        f ((Just (TreeNode _ _ v)):xs) = v : f xs
