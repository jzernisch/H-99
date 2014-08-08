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

  describe "pack (Problem 9)" $ do

    it "packs consecutive duplicates into sublists" $
      pack [1,1,2,3,3,3,1,2] `shouldBe` [[1,1],[2],[3,3,3],[1],[2]]

  describe "encode (Problem 10)" $ do

    it "returns the so-called run-length encoding of a list" $
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

  describe "encodeModified (Problem 11)" $ do

    it "returns the so-called run-length encoding of a list in a modified version" $
      encodeModified "aaaabcc" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c']

  describe "decodeModified (Problem 12)" $ do

    it "decodes a list encoded with encodeModified" $
      decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c'] `shouldBe` "aaaabcc"

  describe "encodeDirect (Problem 13)" $ do

    it "returns the so-called run-length encoding of a list in a modified version" $
      encodeDirect "aaaabcc" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c']

  describe "dupli (Problem 14)" $ do

    it "duplicates each element of a list" $
      dupli "abcca" `shouldBe` "aabbccccaa"

  describe "repli (Problem 15)" $ do

    it "replicates each element of a list a given number of times" $
      repli "abbc" 3 `shouldBe` "aaabbbbbbccc"

  describe "dropEvery (Problem 16)" $ do

    it "drops every n-th element of a list" $
      dropEvery "abcdefgh" 3 `shouldBe` "abdegh"

  describe "split (Problem 17)" $ do

    it "splits a given list into two parts where the first has a given length" $
      split "myname" 2 `shouldBe` ("my","name")

  describe "slice (Problem 18)" $ do

    it "extracts a slice from a list for given indices (1-indexed)" $
      slice "acoolthing" 2 5 `shouldBe` "cool"

  describe "rotate (Problem 19)" $ do

    it "rotates a list n places to the left" $
      rotate "abcde" 2 `shouldBe` "cdeab"

    it "works also for negative values" $
      rotate "abcde" (-1) `shouldBe` "eabcd"

  describe "removeAt (Problem 20)" $ do

    it "removes from a list the element of a given index" $
      removeAt "abcde" 2 `shouldBe` ('b',"acde")

  describe "insertAt (Problem 21)" $ do

    it "inserts an element at a given position into a list" $
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"




