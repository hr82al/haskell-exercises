module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

undold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f start = let
  tmp = f start
  in
    if isNothing tmp
    then Leaf 
    else let
      (Just (v1, v2, v3)) = tmp
    in
      Node (unfold f v1) v2 (unfold f v3)
