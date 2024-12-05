{-# OPTIONS_GHC -Wall #-}
module Antoshchuk01 where

-- Задача 1 -----------------------------------------
productMy :: [Int] -> Int
productMy [] = 1
productMy (x:xs) = x * productMy xs

-- Задача 2 -----------------------------------------
zipMy :: [Int] -> [Int] -> [(Int,Int)]
zipMy xs ys = if null xs || null ys then []
    else (head xs, head ys) : zipMy (tail xs) (tail ys)

-- Задача 3 -----------------------------------------
takeMy :: Int -> [Int] -> [Int]
takeMy n xs = if n <= 0 || null xs then []
    else head xs : takeMy (n - 1) (tail xs)

-- Задача 4 -----------------------------------------
lookupMy :: Int -> [(Int,Int)] -> Int
lookupMy x pxs = if null pxs then -1
    else if fst (head pxs) == x then snd (head pxs)
         else lookupMy x (tail pxs)

-- Задача 5 -----------------------------------------
initMy :: [Int] -> [Int]
initMy xs = if null xs || null (tail xs) then []
    else head xs : initMy (tail xs)
