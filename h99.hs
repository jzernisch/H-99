module H99 where

  import Data.Maybe

  -- Problem 1

  myLast :: [a] -> Maybe a
  myLast [] = Nothing
  myLast [x] = Just x
  myLast (_:xs) = myLast xs


  -- Problem 2

  myButLast :: [a] -> Maybe a
  myButLast [] = Nothing
  myButLast [x] = Nothing
  myButLast [x,_] = Just x
  myButLast (_:xs) = myButLast xs


  -- Problem 3

  elementAt :: [a] -> Int -> Maybe a
  elementAt [] _ = Nothing
  elementAt (x:_) 1 = Just x
  elementAt (_:xs) n = elementAt xs (n - 1)


  -- Problem 4

  myLength :: [a] -> Int
  myLength [] = 0
  myLength (_:xs) = 1 + myLength xs


  -- Problem 5

  myReverse :: [a] -> [a]
  myReverse = foldl (flip (:)) []


  -- Problem 6

  isPalindrome :: Eq a => [a] -> Bool
  isPalindrome x = x == myReverse x
