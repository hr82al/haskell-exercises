module ItsOnlyNatural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n 
  | n < 0 = Nothing
  | otherwise = Just (go n)
    where 
      go 0 = Zero
      go n = Succ (go (n - 1))
