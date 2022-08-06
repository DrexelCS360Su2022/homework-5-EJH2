{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit 0 = 0
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n | n < 0 = []
           | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper [n] = [n]
doubleEveryOtherHelper (x:s:xs) = x : 2 * s : doubleEveryOtherHelper xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherHelper (reverse n))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [n] | n > 9     = lastDigit n + dropLastDigit n
              | otherwise = n
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

validate :: Integer -> Bool
validate n = lastDigit (sumDigits (doubleEveryOther (toDigits n))) == 0

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow _ 0 = id
pow f 1 = f
pow f n = f . (pow f (n - 1))

g :: Integer -> Integer
g 0 = 0
g n = n - ((pow g 2) (n - 1))

h :: Integer -> Integer
h 0 = 0
h n = n - ((pow h 3) (n - 1))

d :: Int -> Integer -> Integer
d _ 0 = 0
d i n = n - ((pow (d i) i) (n - 1))

--
-- Problem 3
--

getFirst :: Ord a => Set a -> a
getFirst xs = (fst (Set.split xs))

getRest :: Ord a => Set a -> Set a
getRest xs = (snd (Set.split xs))

powerSetHelper :: Ord a => Set a -> Set (Set a) -> Set (Set a)
powerSetHelper xs ys | Set.isEmpty ys = (Set.singleton xs)
                     | otherwise      = (Set.union (Set.singleton (Set.union xs (getFirst ys))) (powerSetHelper xs (getRest ys)))

powerSet :: Ord a => Set a -> Set (Set a)
powerSet xs | Set.isEmpty xs     = (Set.singleton Set.empty)
            | (Set.size xs) == 1 = (Set.insert xs (Set.singleton Set.empty))
            | otherwise          = (Set.union (powerSet (getRest xs)) (powerSetHelper (Set.singleton (getFirst xs)) (powerSet (getRest xs))))
