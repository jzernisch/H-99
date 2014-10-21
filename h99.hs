module H99 where

  import Data.Maybe
  import System.Random (StdGen, randomRs, randomR)
  import Data.List (nub, (\\), sortBy)

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


  -- Problem 10

  encode :: Eq a => [a] -> [(Int,a)]
  encode xs = map makeTuple (pack xs)
    where makeTuple xs = (length xs, head xs)


  -- Problem 11

  data ElementCount a = Single a | Multiple Int a deriving (Show, Eq)

  encodeModified :: Eq a => [a] -> [ElementCount a]
  encodeModified = map elementCount . encode
    where elementCount (1, element) = Single element
          elementCount (n, element) = Multiple n element


  -- Problem 12

  decodeModified :: Eq a => [ElementCount a] -> [a]
  decodeModified = concatMap replicatedElement
    where replicatedElement (Single element) = replicate 1 element
          replicatedElement (Multiple n element) = replicate n element


  -- Problem 13

  encodeDirect :: Eq a => [a] -> [ElementCount a]
  encodeDirect [] = []
  encodeDirect [x] = [Single x]
  encodeDirect (x:y:xs)
    | x == y    = (incrElement $ head encodedTail):(tail encodedTail)
    | otherwise = (Single x):encodedTail
    where encodedTail = encodeDirect (y:xs)
          incrElement (Single x) = (Multiple 2 x)
          incrElement (Multiple n x) = (Multiple (n+1) x)


  -- Problem 14

  dupli :: [a] -> [a]
  dupli = foldr (\x acc -> (x:x:acc)) []


  -- Problem 15

  repli :: [a] -> Int -> [a]
  repli xs n = foldr (\x -> (replicate n x ++)) [] xs


  -- Problem 16

  dropEvery :: [a] -> Int -> [a]
  dropEvery [] n = []
  dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n


  -- Problem 17

  split :: [a] -> Int -> ([a],[a])
  split xs 0 = ([],xs)
  split (x:xs) n = (x:(fst splittedTail), snd splittedTail)
    where splittedTail = split xs (n-1)


  -- Problem 18

  slice :: [a] -> Int -> Int -> [a]
  slice [] _ _ = []
  slice xs 1 n = take n xs
  slice xs i k = slice (tail xs) (i-1) (k-1)


  -- Problem 19

  rotate :: [a] -> Int -> [a]
  rotate xs n
    | n < 0     = rotate xs (n + length xs)
    | otherwise = drop n xs ++ take n xs


  -- Problem 20

  removeAt :: [a] -> Int -> (a,[a])
  removeAt (x:xs) 1 = (x, xs)
  removeAt (x:xs) n = (fst resultForTail, x:(snd resultForTail))
    where resultForTail = removeAt xs (n-1)


  -- Problem 21

  insertAt :: a -> [a] -> Int -> [a]
  insertAt y xs 1 = (y:xs)
  insertAt y (x:xs) n = x:(insertAt y xs (n-1))


  -- Problem 22

  range :: Int -> Int -> [Int]
  range a b = [a..b]


  -- Problem 23

  rndSelect :: [a] -> Int -> StdGen -> [a]
  rndSelect xs n g = map (xs!!) randomIndices
    where randomIndices = take n $ randomRs (0, length xs - 1) g


  -- Problem 24

  rndSelectFromRange :: Int -> Int -> StdGen -> [Int]
  rndSelectFromRange k n g = rndSelectFromList k [1..n] g

  rndSelectFromList :: Int -> [a] -> StdGen -> [a]
  rndSelectFromList k xs g = map (xs!!) randomIndices
    where randomIndices = take k $ nub $ randomRs (0, length xs - 1) g


  -- Problem 25

  rndPerm :: [a] -> StdGen -> [a]
  rndPerm xs g = rndSelectFromList (length xs) xs g


  -- Problem 26

  combinations :: Eq a => Int -> [a] -> [[a]]
  combinations k all@(x:xs)
    | k < 0             = []
    | k == 0            = [[]]
    | k > length all    = []
    | k == 1            = map (:[]) all
    | k == length all   = [all]
    | otherwise         = map (x:) (combinations (k-1) xs) ++ filter (not . elem x) (combinations k xs)


  -- Problem 27

  group :: Eq a => [Int] -> [a] -> [[[a]]]
  group _ [] = [[]]
  group (n:ns) xs = [ p:q | p <- combinations n xs, q <- group ns (xs \\ p) ]


  -- Problem 28a

  lsort :: [[a]] -> [[a]]
  lsort xs = sortBy (\x y -> compare (length x) (length y)) xs


  -- Problem 28b

  lfsort :: [[a]] -> [[a]]
  lfsort xs = sortBy (\x y -> compare (lengthFrequency x) (lengthFrequency y)) xs
    where lengthFrequency list = length $ filter (\l -> length l == length list) xs


  -- Problem 31

  isPrime :: Int -> Bool
  isPrime 1 = False
  isPrime n = not $ any (\k -> n `mod` k == 0) [2..(n `div` 2)]


  -- Problem 32

  myGCD :: Int -> Int -> Int
  myGCD a 0 = a
  myGCD a b = myGCD b $ abs (a `mod` b)
