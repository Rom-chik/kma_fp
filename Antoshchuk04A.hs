{-# OPTIONS_GHC -Wall #-}
module Antoshchuk04A where

type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph gr = all (\adj -> all (< length gr) adj && not (hasDuplicates adj)) gr
  where
    hasDuplicates [] = False
    hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

-- Задача 2 ------------------------------------
isTournament :: Graph -> Bool 
isTournament gr = all checkPair (allPairs [0..length gr - 1])
  where
    checkPair (vi, vj) = (vj `elem` (gr !! vi)) /= (vi `elem` (gr !! vj))
    allPairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive gr = all (\v -> all (\u -> all (\w -> w `elem` (gr !! v) || not (w `elem` (gr !! u))) (gr !! u)) (gr !! v)) [0..length gr - 1]


-- Задача 4 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr a b = checkLongWay (allWays gr a) b 
  where 
    checkLongWay :: [[[Int]]] -> Int -> Maybe [Int]
    checkLongWay [] _ = Nothing
    checkLongWay [[xh]] t 
      | head xh == t = Just xh
      | otherwise = Nothing
    checkLongWay ([]:xss) t = checkLongWay xss t
    checkLongWay ((xh:xt):xss) t 
      | head xh == t = Just (reverse xh) 
      | otherwise = checkLongWay (xt:xss) t

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: [[[Int]]] -> Bool
condW wss = null (head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs)<-wsn, notElem x xs, t <- gr!!x] : wss
stepW _ [] = error "allWays:StepW"

-- Задача 5 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = 
  let allWaysRes = reverse (allWays gr 0)
  in if length allWaysRes < length gr + 1
    then Nothing
    else findGamiltonWay (reverse (allWaysRes !! (length gr)))
  where 
    findGamiltonWay :: [[Int]] -> Maybe [Int]
    findGamiltonWay [] = Nothing
    findGamiltonWay [x] 
      | head x == 0 = Just (reverse x)
      | otherwise = Nothing
    findGamiltonWay (x:xs) 
      | head x == 0 = Just (reverse x)
      | otherwise = findGamiltonWay xs

-- Задача 6 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = not (any (\v -> visit v []) [0..length gr - 1])
  where
    visit v visited = v `elem` visited || any (`visit` (v:visited)) (gr !! v)

-- Задача 7 ------------------------------------
topolSort :: Graph -> Maybe [Int]
topolSort gr
       | isAcyclic gr = Just (topologicalSort gr)
       | otherwise = Nothing

topologicalSort :: Graph -> [Int]
topologicalSort gr = (dfs [] gr)

dfs :: [Int] -> Graph -> [Int]
dfs visited gr
       | length visited == length gr = visited
       | otherwise = dfs (next : visited) gr
       where
              next = head [v | v <- [0..length gr - 1], v `notElem` visited, all (`elem` visited) (gr !! v)]

-- Задача 8 ------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort gr ts = all (\(vi, vj) -> findIndex vi ts < findIndex vj ts) [(vi, vj) | vi <- [0..length gr - 1], vj <- gr !! vi]
  where
    findIndex :: Int -> [Int] -> Int
    findIndex _ [] = -1
    findIndex x (y:ys) = if x == y then 0 else 1 + findIndex x ys

-- Test data
gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]
