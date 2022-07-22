module SmalLibraryForMaybe where


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee _ f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a 
listToMaybe [] = Nothing
listToMaybe x = Just (head x) 

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
  where
    go Nothing b = b
    go (Just a) b = a : b

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if all isJust xs 
  then Just $ foldr (\(Just a) b -> a : b) [] xs
  else Nothing

