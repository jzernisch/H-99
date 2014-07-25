module H99Spec where

import Test.Hspec
import H99

main :: IO ()
main = hspec $ do

  describe "myLast (Problem 1)" $ do

    it "returns the last element of a list" $
      myLast [1,2,3] `shouldBe` 3

    it "throws an error when called with an empty list" $
      myLast [] `shouldThrow` anyErrorCall