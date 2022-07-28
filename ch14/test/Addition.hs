module Addition where

import Test.Hspec
import Test.QuickCheck
-- import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "dasdf" $ do
    it "x" $ do
      property $ \x -> x + 1 > (x:: Int)
-- main = hspec $ do
  -- describe "Addition" $ do
  -- it "1 + 1 is greater then 1" $ do
  --   (1 + 1) > (1 :: Integer) `shouldBe` True
  -- it "2 + 2 is equal to 4" $ do
  --   2 + 2 `shouldBe` (4 :: Integer)
