{-# OPTIONS_GHC -Wall #-}
module Antoshchuk08 where

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнє 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 ------------------------------------
checkAll :: (Ord a) => BinTreeM a -> a -> (a -> a -> Bool) -> Bool
checkAll EmptyM _ _ = True
checkAll (NodeM h _ l r) element comp = comp h element && checkAll l element comp && checkAll r element comp

isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM h _ l r) = checkAll l h (<) && checkAll r h (>) && isSearch l && isSearch r


-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v _ l r) x
    | x == v = True
    | x < v = elemSearch l x
    | otherwise = elemSearch r x


-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM x = NodeM x 1 EmptyM EmptyM
insSearch (NodeM v n l r) x
    | x == v = NodeM v (n + 1) l r
    | x < v = NodeM v n (insSearch l x) r
    | otherwise = NodeM v n l (insSearch r x)


-- Задача 4 ------------------------------------
minElem :: (Ord a) => BinTreeM a -> a
minElem EmptyM = error "EMPTY TREE"
minElem (NodeM h _ EmptyM _) = h
minElem (NodeM _ _ t1 _) = minElem t1

delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _ = EmptyM
delSearch (NodeM h _ t1 EmptyM) v
    | v == h    = t1
    | v < h     = NodeM h 1 (delSearch t1 v) EmptyM
    | otherwise = NodeM h 1 t1 EmptyM
delSearch (NodeM h _ EmptyM t2) v
    | v == h    = t2
    | v < h     = NodeM h 1 EmptyM t2
    | otherwise = NodeM h 1 EmptyM (delSearch t2 v)
delSearch (NodeM h _ t1 t2) v
    | v < h     = NodeM h 1 (delSearch t1 v) t2
    | v > h     = NodeM h 1 t1 (delSearch t2 v)
    | otherwise = NodeM m 1 t1 (delSearch t2 m)
  where
    m = minElem t2


-- Задача 5 ------------------------------------
treeToList :: (Ord a) => BinTreeM a -> [a]
treeToList EmptyM = []
treeToList (NodeM h count t1 t2) = treeToList t1 ++ replicate count h ++ treeToList t2

sortList :: (Ord a) => [a] -> [a]
sortList xs = treeToList (foldl insSearch EmptyM xs)


-- Задача 6-----------------------------------------
height :: Tree23 a -> Int
height Empty23 = 0
height (Leaf _) = 1
height (Node2 t1 _ t2) = 1 + max (height t1) (height t2)
height (Node3 t1 _ t2 _ t3) = 1 + maximum [height t1, height t2, height t3]

check23 :: (Ord a) => Tree23 a -> a -> (a -> a -> Bool) -> Bool
check23 Empty23 _ _ = True
check23 (Leaf e) key comp = comp e key
check23 (Node2 t1 _ t2) key comp = check23 t1 key comp && check23 t2 key comp
check23 (Node3 t1 _ t2 _ t3) key comp = check23 t1 key comp && check23 t2 key comp && check23 t3 key comp

isTree23 :: (Ord a) => Tree23 a -> Bool
isTree23 Empty23 = True
isTree23 (Leaf _) = True
isTree23 (Node2 t1 h t2) = check23 t1 h (<=) && check23 t2 h (>=) && height t1 == height t2
isTree23 (Node3 t1 h1 t2 h2 t3) = check23 t1 h1 (<=) && check23 t2 h1 (>=) && check23 t2 h2 (<=) && check23 t3 h2 (>=) && height t1 == height t2 && height t1 == height t3



-- Задача 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Empty23 _ = False
elemTree23 (Leaf e) key = key == e
elemTree23 (Node2 t1 h t2) key
    | key == h  = elemTree23 t1 key || elemTree23 t2 key
    | key < h   = elemTree23 t1 key
    | otherwise = elemTree23 t2 key
elemTree23 (Node3 t1 h1 t2 h2 t3) key
    | key < h1  = elemTree23 t1 key
    | key == h1 = elemTree23 t1 key || elemTree23 t2 key
    | key == h2 = elemTree23 t2 key || elemTree23 t3 key
    | key < h2  = elemTree23 t2 key
    | otherwise = elemTree23 t3 key


-- Задача 8-----------------------------------------
tree23ToList :: (Ord a) => Tree23 a -> [a]
tree23ToList Empty23 = []
tree23ToList (Leaf e) = [e]
tree23ToList (Node2 t1 _ t2) = tree23ToList t1 ++ tree23ToList t2
tree23ToList (Node3 t1 _ t2 _ t3) = tree23ToList t1 ++ tree23ToList t2 ++ tree23ToList t3

eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = tree23ToList t1 == tree23ToList t2


-- Задача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 tr v =    let res = insert v tr
                    in case snd res of
                        Nothing -> fst res
                        Just pair ->
                                let f = fst pair in
                                let s = snd pair in    
                                Node2 (fst res) f s

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  Бінарні дерева 
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

---- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )

