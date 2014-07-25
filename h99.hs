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
