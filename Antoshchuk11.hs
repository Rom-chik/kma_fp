{-# OPTIONS_GHC -Wall #-}
module Antoshchuk11 where

--  Пакет parsec може бути закритим (hidden), 
--  щоб відкрити його потрібно завантажити файл з опцією -package parsec
--  ghci xxxx.hs -package parsec // ghc xxxx.hs -package parsec 

import Text.Parsec.String 
import Text.Parsec   --- parse
import Data.Char(isLower)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])


-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat bdd env = if fst bdd == 0 then False else if fst bdd == 1 then True
                     else let nodes = [n | n@(ind, _) <- snd bdd, ind == fst bdd]
                              (_, (nm, f, t)) = if null nodes then (-1, (' ', -1, -1)) else head nodes
                              nextId = let envs = [e | e <- env, fst e == nm]
                                       in if null envs then -1 else if (snd $ head envs) then t else f
                          in if nextId == -1 then False else checkSat (nextId, snd bdd) env


-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat (root, nodes) = findSat root
  where
    findSat :: NodeId -> [[(Char, Bool)]]
    findSat 0 = []
    findSat 1 = [[]]
    findSat n = case lookup n nodes of
      Just (var, low, high) ->
        let lowSat  = map ((var, False):) (findSat low)
            highSat = map ((var, True):) (findSat high)
        in lowSat ++ highSat
      Nothing -> error "no node"


-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify e = case e of 
               Or (Bvalue a) (Bvalue b) -> Bvalue $ a || b
               Not (Bvalue x) -> Bvalue $ not x
               And (Bvalue a) (Bvalue b) -> Bvalue $ a && b
               _ -> e


-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict e x v = let f :: BExp -> BExp
                     f ee = case ee of 
                              Bvar y -> if y == x then Bvalue v else ee
                              Or a b -> simplify $ Or (f a) (f b)
                              And a b -> simplify $ And (f a) (f b)
                              Not a -> simplify $ Not $ f a
                              _ -> ee
                 in f e


-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' e _ []
  | e == Bvalue False = (0, [])
  | otherwise = (1, [])
buildBDD' e n (x:xs) = (n,(n,(x, lid, rid)) : lnodes ++ rnodes)
    where (lid, lnodes) = buildBDD' (restrict e x False) (2*n) xs
          (rid, rnodes) = buildBDD' (restrict e x True) (2*n+1) xs


-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD = undefined


-- Задача 7 -----------------------------------------
fullBexp :: String -> Maybe BExp
fullBexp input = case parse bexpParser "" input of
                   Left _ -> Nothing
                   Right expr -> Just expr

-- Parser for a Boolean expression
bexpParser :: Parser BExp
bexpParser = try orParser

-- Parser for OR expressions
orParser :: Parser BExp
orParser = chainl1 andParser (char '|' >> return Or)

-- Parser for AND expressions
andParser :: Parser BExp
andParser = chainl1 notParser (char '&' >> return And)

-- Parser for NOT expressions and primary elements
notParser :: Parser BExp
notParser = (char '!' >> fmap Not primaryParser) <|> primaryParser

-- Parser for basic values and variables
primaryParser :: Parser BExp
primaryParser = (char 'T' >> return (Bvalue True))
             <|> (char 'F' >> return (Bvalue False))
             <|> (Bvar <$> lower)
             <|> between (char '(') (char ')') bexpParser

------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])
