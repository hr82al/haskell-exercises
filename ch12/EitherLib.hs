module EitherLib where
import Data.Either (partitionEithers)

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where 
    f (Left a) b = a : b
    f (Right _) b = b

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right a) b = a : b
    f (Left _) b = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([],[])
  where 
    f (Left a) (bl, br) = (a : bl, br)
    f (Right a) (bl, br) = (bl, a : br)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

