module H99Spec where

import Test.Hspec
import H99

main :: IO ()
main = hspec $ do

  describe "myLast (Problem 1)" $ do

    it "returns the last element of a list" $
      myLast [1,2,3,4] `shouldBe` Just 4

    it "returns Nothing when called with an empty list" $
      myLast [] `shouldBe` (Nothing::(Maybe Int))

  describe "myButLast (Problem 2)" $ do

    it "returns the last but one element of a list" $
      myButLast [1,2,3,4] `shouldBe` Just 3

    it "returns Nothing when called with a singleton list" $
      myButLast [1] `shouldBe` Nothing

    it "returns Nothing when called with an empty list" $
      myButLast [] `shouldBe` (Nothing::(Maybe Int))

  describe "elementAt (Problem 3)" $ do

    it "returns the element at the given index (1-indexed)" $
      elementAt [3,4,1] 2 `shouldBe` Just 4

    it "returns Nothing when the index is greater the length of the list" $
      elementAt [3,4,1] 4 `shouldBe` Nothing

  describe "myLength (Problem 4)" $ do

    it "returns the length of a list" $
      myLength [1,2,3] `shouldBe` 3

  describe "myReverse (Problem 5)" $ do

    it "reverses a list" $
      myReverse [1,2,3] `shouldBe` [3,2,1]

  describe "isPalindrome (Problem 6)" $ do

    it "returns True if the list is a palindrome" $
      isPalindrome [1,2,3,2,1] `shouldBe` True

    it "returns False if the list is not a palindrome" $
      isPalindrome [1,2,3,5,1] `shouldBe` False

  describe "flatten (Problem 7)" $ do

    it "flattens a NestedList and returns a list" $
      flatten (List [Elem 4, List [Elem 2, List [Elem 1, Elem 5]], Elem 3]) `shouldBe` [4,2,1,5,3]

  describe "compress (Problem 8)" $ do

    it "removes consecutive duplicates from a list" $
      compress [1,1,2,3,3,3,1,2] `shouldBe` [1,2,3,1,2]
