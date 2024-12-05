{-# OPTIONS_GHC -Wall #-}
module Antoshchuk02 where

-- Задача 1 -----------------------------------------
productFl :: [Int] -> Int
productFl  = foldl (*) 1
  
-- Задача 2 ----------------------------------------- 
andFl :: [Bool] -> Bool
andFl  = foldl (&&) True

-- Задача 3 -----------------------------------------
maximumFr :: [Int] -> Int
maximumFr  = foldr1 max 

-- Задача 4 -----------------------------------------
tails :: [Int] -> [[Int]]
tails  = scanr (:) []

-- Задача 5 -----------------------------------------
allFirst :: [String] -> String
allFirst = map head . filter (not . null)
