{-# OPTIONS_GHC -Wall #-}
module Antoshchuk03 where

-- Задача 1 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart = undefined

-- Задача 2 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [n^2 | n <- [1..]]

-- Задача 3 -----------------------------------------
testing :: [Int] -> Bool
testing xs = if null xs || length xs == 1 then True
    else if head xs <= head (tail xs) then testing (tail xs)
        else False

-- Задача 4 -----------------------------------------
primeCnt :: [Int] -> Int
primeCnt xs = length (filter isPrime (filter (> 1) xs))
    where isPrime x = if x < 2 then False
                else null [d | d <- [2..(x - 1)], x `mod` d == 0]
  
-- Задача 5 ----------------------------------------- 
compress :: [Int] -> [Int]
compress xs = if null xs then []
    else let x = head xs in x : compress (dropWhile (== x) (tail xs))

-- Задача 6 -----------------------------------------
primeFactor :: Int -> [Int]
primeFactor n = factorize n 2
  where
    factorize n f = 
      if n == 1 then []
      else if n `mod` f == 0 then f : factorize (n `div` f) f
           else factorize n (f + 1)

-- Задача 7 -----------------------------------------
intToString :: Int -> Int -> String
intToString base num = if num < base then [digits !! num]
    else intToString base (num `div` base) ++ [digits !! (num `mod` base)]
    where digits = "0123456789abcdef"

-- Задача 8 -----------------------------------------
sumPalindrom2 :: Integer -> Integer
sumPalindrom2 n = sum [x | x <- [1..n], isBinaryPalindrome x]
  where
    isBinaryPalindrome x = let bin = toBinary x in bin == reverse bin
    toBinary 0 = "0"
    toBinary x = reverse (toBinary' x)
    toBinary' 0 = ""
    toBinary' x = show (x `mod` 2) ++ toBinary' (x `div` 2)
