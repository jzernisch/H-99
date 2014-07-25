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


  -- Problem 7

  data NestedList a = Elem a | List [NestedList a]

  flatten :: NestedList a -> [a]
  flatten (Elem x) = [x]
  flatten (List []) = []
  flatten (List (x:xs)) = flatten x ++ flatten (List xs)


  -- Problem 8

  compress :: Eq a => [a] -> [a]
  compress [] = []
  compress [x] = [x]
  compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = x:(compress (y:xs))


  -- Problem 9

  pack :: Eq a => [a] -> [[a]]
  pack [] = []
  pack [x] = [[x]]
  pack (x:y:xs)
    | x == y    = (x:(head packedTail)):(tail packedTail)
    | otherwise = [x]:packedTail
    where packedTail = pack (y:xs)
