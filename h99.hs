module H99 where

  -- Problem 1

  myLast :: [a] -> a
  myLast [] = error "List is empty!"
  myLast [x] = x
  myLast (_:xs) = myLast xs

