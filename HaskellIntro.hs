{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set
import Control.Concurrent (yield)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit x = mod x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

toDigits :: Integer -> [Integer]
toDigits x
    |   x <= 0    = []
    |   otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : y : z) = (x * 2) : y : doubleEveryOther z

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : []) = x
sumDigits (x : xs) = (lastDigit x) + (dropLastDigit x) + sumDigits xs

validate :: Integer -> Bool
validate x
    |   sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0 = True
    |   otherwise = False

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f n x
    |   n == 0 = x
    |   otherwise = f (pow f (n - 1) x)

g :: Integer -> Integer
g n
    |   n == 0 = 0
    |   otherwise = n - (pow g 2 n-1)

h :: Integer -> Integer
h n
    |   n == 0 = 0
    |   otherwise = n - (pow h 3 n-1)

d :: Int -> Integer -> Integer
d i n
    |   i == 0 = 0
    |   otherwise = n - d i (pow g 2 n-1)

--
-- Problem 3
--

powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet s
    |   isEmpty s = singleton empty
    |   otherwise = singleton empty