{-# OPTIONS_GHC -Wall #-}
module Antoshchuk07 where

newtype Poly a = P [a]

-- Задача 1 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    P xs == P ys = dropWhile (== 0) xs == dropWhile (== 0) ys
 
-- Задача 2 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
     show (P coeffs) = unwords $ reverse $ formatTerms $ reverse coeffs
      where
        formatTerms [] = []
        formatTerms (c:cs) = formatTerm c (length cs) : formatTerms cs
        formatTerm 0 _ = ""
        formatTerm c 0 = show c
        formatTerm c 1
          | c == 1    = "x"
          | c == -1   = "-x"
          | otherwise = show c ++ "x"
        formatTerm c e
          | c == 1    = "x^" ++ show e
          | c == -1   = "-x^" ++ show e
          | otherwise = show c ++ "x^" ++ show e

-- Задача 3 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P $ zipWith (+) (extend xs) (extend ys)
  where
    extend zs = zs ++ replicate (max (length xs) (length ys) - length zs) 0

-- Задача 4 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = P (foldl addTerm (replicate (length xs + length ys - 1) 0) indexedTerms)
  where
    indexedTerms = [(i + j, x * y) | (i, x) <- zip [0..] xs, (j, y) <- zip [0..] ys]
    addTerm acc (index, value) = take index acc ++ [acc !! index + value] ++ drop (index + 1) acc

-- Задача 5 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map negate xs)
    fromInteger n = P [fromInteger n]
    abs    = undefined
    signum = undefined

-- Задача 6 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P coeffs) x = sum $ zipWith (*) coeffs (map (x^) [0..])

-- Задача 7 -----------------------------------------
class Num a => Differentiable a where
    derive  :: a -> a
    nderive :: Int -> a -> a
    nderive 0 p = p
    nderive n p = derive (nderive (n - 1) p)

-- Задача 8 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    derive (P coeffs) = P $ zipWith (*) (map fromInteger [1..]) (tail coeffs)
