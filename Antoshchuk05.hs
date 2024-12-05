{-# OPTIONS_GHC -Wall #-}
module Antoshchuk05 where

import Data.Char (isUpper)
import Data.List (nub, sort)

type Grammar = [Production]
type Production = (Char,String)
-- Граматика - список продукцій.
--   нетермінал першої - початковий

-- Лівосторонній вивід - послідовність слів  [String] з іншого боку
--     послідовність номерів правил, які при цьому застосовувались [Int]
type DerivationS = [String] 
type DerivationR = [Int] 


myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition _ [] = ([], [])
myPartition p (x:xs)
  | p x       = (x : ys, zs)
  | otherwise = (ys, x : zs)
  where
    (ys, zs) = myPartition p xs


-- Задача 1 -----------------------------------------
isGrammar ::  Grammar -> Bool
isGrammar [] = True
isGrammar gr = all (isUpper . fst) gr

-- Задача 2.a ---------------------------------------
allTerm :: Grammar -> String
allTerm = nub . sort . filter isTerm . concatMap snd
  where isTerm c = not (isUpper c)

-- Задача 2.b ---------------------------------------
allNotT :: Grammar -> String
allNotT = nub . sort . map fst

-- Задача 2.c ---------------------------------------
newMinN :: Grammar -> Char
newMinN gr = head $ filter (`notElem` allNotT gr) ['A'..'Z']

-- Задача 3.a -----------------------------------------
buildGen :: Grammar -> String
buildGen gr = go []
  where
    go productive
      | productive' /= productive = go productive'
      | otherwise = nub productive
      where
        productive' = productive ++ [n | (n, rhs) <- gr,
                                         all (`elem` (productive ++ allTerminals)) rhs,
                                         n `notElem` productive]
    allTerminals = [c | (_, rhs) <- gr, c <- rhs, not (isUpper c)]

-- Задача 3.b -----------------------------------------
buildAcc :: Grammar -> String
buildAcc gr = nub $ sort $ go [] ['S']
  where
    go acc [] = acc
    go acc (n:ns)
      | n `elem` acc = go acc ns
      | otherwise = go (n:acc) (ns ++ [t | (nt, rhs) <- gr, nt == n, t <- rhs, isUpper t])

-- Задача 3.c -----------------------------------------
reduce :: Grammar -> Grammar
reduce gr = [p | p@(n, _) <- gr, n `elem` gen, n `elem` acc]
  where
    gen = buildGen gr
    acc = buildAcc gr

-- Задача 4.a -----------------------------------------
findLeftR :: Grammar -> String
findLeftR [] = []
findLeftR ((n, r):ps)
    | n == head r = findLeftR ps ++ [n]
    | otherwise   = findLeftR ps

-- Задача 4.b -----------------------------------------
deleteLeftR :: Grammar -> Char -> Grammar
deleteLeftR gr n =
  let (leftRec, nonRec) = myPartition (\(lhs, rhs) -> lhs == n && head rhs == n) gr
      newNonRec = [(lhs, rhs) | (lhs, rhs) <- nonRec] ++ 
                   [(n, drop 1 rhs) | (lhs, rhs) <- leftRec]  
      newRec = [(newN, rhs ++ [newN]) | (_, rhs) <- leftRec] ++ [(newN, "")]
      newN = newMinN gr  
  in newNonRec ++ newRec

-- Задача 5.a -----------------------------------------
isFact :: Grammar -> Char -> Bool
isFact gr n =
  let prefixes = [take 1 rhs | (lhs, rhs) <- gr, lhs == n] 
  in length (filter (\p -> length (filter (== p) prefixes) > 1) prefixes) > 0

-- Задача 5.b -----------------------------------------
deleteFact :: Char -> String -> Grammar -> Grammar
deleteFact n p gr =
  let (factored, nonFactored) = myPartition (\(lhs, rhs) -> lhs == n && take (length p) rhs == p) gr
      newNonFactored = [(lhs, drop (length p) rhs) | (lhs, rhs) <- factored] ++ nonFactored
      newN = newMinN gr  
      newFactored = [(newN, drop (length p) rhs ++ [newN]) | (_, rhs) <- factored]
  in newNonFactored ++ newFactored ++ [(newN, "")]

-- Задача 6.a -----------------------------------------
isLeftDerivationS :: Grammar -> DerivationS -> Bool
isLeftDerivationS = undefined

-- Задача 6.b -----------------------------------------
isLeftDerivationR :: Grammar -> DerivationR -> Bool
isLeftDerivationR = undefined

-- Задача 7 -----------------------------------------
fromLeftR :: Grammar -> DerivationR -> DerivationS
fromLeftR = undefined

--------------------------------------------------------
--  тестові дані 
gr0, gr1, gr1e, gr2 :: Grammar   
gr0 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba")]
gr1 = [ ('S',"aSa"), ('S',"bSd"), ('S',"c"), ('S',"aSb"), ('D',"aC") 
      , ('A',"cBd"), ('A',"aAd"),('B',"dAf"),('C',"cS"), ('C',"a")]
gr1e = [('S',"aAS"), ('S',"a"),('a',"SbA"),('A',"ba"),('S',"")]
gr2 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   

gr0S, gr0Se :: [String]
gr0S = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabbaa"]
gr0Se = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabba"]

gr0R :: DerivationR
gr0R = [0, 2, 1, 3, 1]



