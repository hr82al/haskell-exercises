module IterateUnfoldr where
import Data.Maybe (isNothing)

myIterate :: (a -> a) -> a -> [a]
myIterate f start = start : myIterate f (f start)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f start = 
  let
    tmp = f start
    in 
      if isNothing tmp
      then []
      else let
        (Just (x, y)) = tmp
      in 
        x : myUnfoldr f y
        
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y)) 
