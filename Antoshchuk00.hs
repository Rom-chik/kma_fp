{-# OPTIONS_GHC -Wall #-}
module Antoshchuk00 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Rose a = Node a (Forest a) deriving  (Show, Eq)
type Forest a = [Rose a]

-- Задача 1 -----------------------------------------
bagSubbag :: String -> String -> Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (x:xs) w
    | elem x w = bagSubbag xs (delFirst w x)
    | otherwise = False

delFirst :: String -> Char -> String
delFirst [] _ = ""
delFirst (a:b) res
    | a==res = b
    | otherwise = [a] ++ delFirst b res

-- Задача 2 -----------------------------------------
bagUnion :: String -> String -> String
bagUnion a b = compareBags (quicksort a, quicksort b)

-- (subSet,set)
compareBags :: ([Char],[Char]) -> [Char]
compareBags (xs,[]) = xs
compareBags ([],xs) = xs
compareBags (xs,ys) 
    | head xs == head ys = (head xs):(compareBags (tail xs, tail ys))
    | head xs > head ys = (head ys):(compareBags (xs, tail ys))
    | otherwise = (head xs):(compareBags (tail xs, ys))

quicksort :: [Char] -> [Char]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- Задача  3 -----------------------------------------
bagIntersect :: String -> String -> String
bagIntersect st1 st2 = set $ intersect st1 st2

intersect :: String -> String -> String
intersect [] _ = []
intersect (s1:ss1) s2
    | s1 `elem` s2 = s1 : intersect ss1 s2
    | otherwise = intersect ss1 s2

set :: String -> String
set [] = []
set (x:xs) = x:set (filter(/=x) xs)

--- Задача 4 ----------------------------------------
genExpr :: Int -> Int -> [String]
genExpr a b =
  let nums = map charToInt (intToString a 10)
      conbs = permuGenExpr (length nums - 1)
      res = filter (\conb -> evalExpr nums conb == b) conbs
   in map (mergeGenExpr nums) res

mergeGenExpr :: [Int] -> [String] -> String
mergeGenExpr (x : xs) (y : ys) = show x ++ y ++ mergeGenExpr xs ys
mergeGenExpr [] (y : ys) = y ++ mergeGenExpr [] ys
mergeGenExpr (x : xs) [] = show x ++ mergeGenExpr xs []
mergeGenExpr [] [] = []

permuGenExpr :: Int -> [[String]]
permuGenExpr 1 = [["+"], ["-"], ["*"]]
permuGenExpr n = [x : xs | x <- ["+", "-", "*"], xs <- permuGenExpr (n - 1)]

evalExpr :: [Int] -> [String] -> Int
evalExpr [] _ = error "No numbers to evaluate"
evalExpr (a : _) [] = a
evalExpr (a : xs) (op : ops) =
  let b = head xs
      res = eval a b op
   in evalExpr (res : tail xs) ops

eval :: Int -> Int -> String -> Int
eval x y "+" = x + y
eval x y "-" = x - y
eval x y "*" = x * y
eval _ _ op = error $ "Invalid operator: " ++ op

charToInt :: Char -> Int
charToInt c = if c `elem` ['0' .. '9'] then fromEnum c - 48 else fromEnum c - 87

intToString :: Int -> Int -> String
intToString n m = if n < m then [intToChar n] else intToString (n `div` m) m ++ [intToChar (n `mod` m)]

intToChar :: Int -> Char
intToChar n = if n < 10 then toEnum (n + 48) else toEnum (n + 87)

--- Задача 5 ----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket = undefined

-- Задача  6 -----------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (ls, rs, _) i w =
  if take (length ls) (drop i w) == ls
    then take i w ++ rs ++ drop (i + length ls) w
    else w

-- Задача 7 -----------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)]
findPosition w (ls, rs, end) = 
  [((ls, rs, end), i) | i <- [0 .. length w - length ls], take (length ls) (drop i w) == ls]

-- Задача 8 -----------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w = concatMap (\sub@(ls, _, _) -> 
                             if null ls 
                             then [(sub, i) | i <- [0..length w]] 
                             else findPosition w sub) algo

--- Задача 9 ----------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt, st, word)
  | not bt = (bt, st, word) 
  | otherwise = case applyFirst algo word of
      Just (newWord, isFinal) -> (not isFinal, st + 1, newWord)
      Nothing -> (False, st, word)
  where
    applyFirst :: Algorithm -> String -> Maybe (String, Bool)
    applyFirst [] _ = Nothing
    applyFirst ((ls, rs, isFinal):subs) w =
      case findPosition w (ls, rs, isFinal) of
        ((_, pos):_) -> Just (substitute (ls, rs, isFinal) pos w, isFinal)
        [] -> applyFirst subs w

-- Задача 10 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String
evalA algo maxSteps word = evalHelper algo maxSteps (True, 0, word)
  where
    evalHelper :: Algorithm -> Int -> ConfigA -> Maybe String
    evalHelper _ 0 _ = Nothing
    evalHelper _ _ (False, _, w) = Just w
    evalHelper algorithm steps config =
      let newConfig = stepA algorithm config
      in evalHelper algorithm (steps - 1) newConfig

-- Задача 11 -----------------------------------------			   
rank :: Rose a -> Int
rank node = length (getChildren node)

getChildren :: Rose a -> Forest a
getChildren (Node _ children) = children

-- Задача 12-----------------------------------------
calculateNodeNumber:: Rose a -> Int
calculateNodeNumber tree = if (length (getChildren tree) == 0)
                        then 1
                    else 1 + sum (map (\el -> calculateNodeNumber el) (getChildren tree))

isBinomTree :: Ord a => Rose a -> Bool
isBinomTree ts = 2 ^ (rank ts) == calculateNodeNumber ts

-- Задача 13 -----------------------------------------
isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap [] = error "can't be empty"
isBinomHeap [x] = isBinomTree x
isBinomHeap xs = foldl1 (&&) [isBinomTree(xs!!n) && rank(xs!!n) < rank(xs!!(n+1))
                       | n <- [0..(length xs)-2]] && isBinomTree(xs!!((length xs)-1))

-- Задача 14 -----------------------------------------
combineTrees :: Ord a => Rose a -> Rose a -> Rose a
combineTrees (Node x xs) (Node y ys)
    | x < y     = Node x (Node y ys : xs)
    | otherwise = Node y (Node x xs : ys)

-- Задача 15 -----------------------------------------
extractMin :: Ord a => Forest a -> a
extractMin forest = minimum (map getRootLabel forest)

getRootLabel :: Rose a -> a
getRootLabel (Node value _) = value

-- Задача 16-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps [] heap = heap
mergeHeaps heap [] = heap
mergeHeaps h@(t:ts) heap@(t':ts')
  | rank t < rank t' = t : mergeHeaps ts heap
  | rank t' < rank t = t' : mergeHeaps h ts'
  | otherwise = mergeHeaps (mergeHeaps ts ts') [combineTrees t t']

-- Задача 17-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert value forest = mergeHeaps [Node value []] forest

-- Задача 18-----------------------------------------
deleteMin :: Ord a => Forest a -> Forest a
deleteMin heap = mergeHeaps remainingHeap reversedChildren
  where
    mrt = head $ dropWhile (\t -> getRootLabel t /= extractMin heap) heap
    reversedChildren = reverse $ getChildren mrt
    remainingHeap = filter (/= mrt) heap

-- Задача 19-----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = []
binomSort bs = minValue : binomSort (delete (minValue) (bs))
    where
      heap (b:xs) = Node b [] : heap xs
      heap [] = []
      minValue = extractMin $ heap bs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
  | x == y    = ys
  | otherwise = y : delete x ys

-- Задача 20 -----------------------------------------
toBinary :: Forest a -> [Int]
toBinary [] = error "can't be empty"
toBinary f = toBinaryHeap 0 (f,[])

toBinaryHeap :: Int -> (Forest a, [Int]) -> [Int]
toBinaryHeap _ ([],ys) = ys
toBinaryHeap n (xs,ys) | length(getChildren$head xs)==n = toBinaryHeap (n+1) (tail xs, 1:ys) 
                         | otherwise = toBinaryHeap (n+1) (xs, 0:ys)


---------------------Тестові дані - нормальні алгоритми Маркова -------
clearBeginOne, addEnd, reversal, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reversal = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: Rose Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []], 
             Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []], 
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 [Node 6 [Node 8 []], 
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []], 
             Node 7 []]

-- Додаткове дерево...
t8 = Node 12 [Node 16 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: Forest Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]      