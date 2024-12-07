{-# OPTIONS_GHC -Wall #-}
module Antoshchuk10 where 

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne] 
                 deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
getPolInt :: (Int,Rational) -> Int
getPolInt (first, _) = first

getPolRat :: (Int,Rational) -> Rational
getPolRat (_, second) = second

coef :: Rational -> PolinomOne -> PolinomOne 
coef c p0 = if c == 0 || null p0 then []
  else [(getPolInt(head p0), getPolRat(head p0)*c)]++coef(c)(tail p0)


-- Задача 1.b -----------------------------------------
myFilter :: (Int,Rational) -> PolinomOne -> (Int,Rational) 
myFilter (i, r) p1 = if null p1 then (i, r)
  else if getPolInt(head p1) == i then myFilter (i, r+getPolRat(head p1)) (tail p1)
  else myFilter (i, r) (tail p1)

remove :: Int -> PolinomOne -> PolinomOne
remove i p = if null p then []
  else if i == getPolInt(head p) then remove (i) (tail p)
  else [(head p)]++remove (i) (tail p)

plus :: PolinomOne -> PolinomOne
plus p = if null p then []
  else if getPolRat(myFilter(head p)(tail p)) /= 0 then [myFilter(head p)(tail p)]++ plus ( (remove (getPolInt(head p)) (tail p)))
  else plus ((remove (getPolInt(head p)) (tail p)))

sorting :: PolinomOne -> PolinomOne
sorting p = if null p then []
  else if (length (filter (>(head p)) p) == (length p - 1)) then [head p]++sorting (tail p)
  else sorting((filter (<(head p)) p)++[head p]++(filter (>(head p)) p))

add :: PolinomOne -> PolinomOne -> PolinomOne
add p0 p1 = sorting (plus (p0 ++ p1))


-- Задача 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne 
unify p0 = sorting (plus (p0))


-- Задача 2.a -----------------------------------------
polToList :: PolinomOne -> [Int]
polToList p = if null p then []
  else if getPolInt(head p)/= 0 then [getPolInt(head p)]++polToList(tail p)
  else polToList(tail p)

check :: PolinomOne -> Bool
check p = if filter( >= (1,0)) p /= [] && length (filter( >= (1,0)) p)==1 then True
  else False

withoutRepeat :: [Int] -> [Int]
withoutRepeat xs = if null xs then []
  else if length (filter(== head xs) xs) > 1 then withoutRepeat(tail xs)
  else [head xs]++withoutRepeat(tail xs)

findFree1 :: [PolinomOne] -> [Int]
findFree1 pos = if null pos then []
  else if (check (head pos)) then polToList(head pos)++(findFree1 (tail pos))
  else (findFree1 (tail pos))

findFree :: [PolinomOne] -> [Int]
findFree pos = withoutRepeat(findFree1 pos)


-- Задача 2.b -----------------------------------------
commonPar :: [PolinomOne]  -> [Int]
commonPar pos = if null pos then []
  else polToList (head pos) ++ commonPar (tail pos)

equality :: [Int] -> [Int] -> Bool
equality xs ys = if null xs then True
  else if (filter(== head xs) ys) == [] then False
  else equality (tail xs) (ys)

iswfCommon ::  [PolinomOne]  -> Bool 
iswfCommon pos = if (length(findFree pos) == length (withoutRepeat(commonPar pos))) && equality(findFree pos)(commonPar pos) then True
  else False


-- Задача 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = if null le then True
  else if length(head le)> 1 then False
  else isSimple (tail le)


-- Задача 3.b -----------------------------------------
solveSimple :: Linear -> Maybe [PolinomOne]
solveSimple le = if (filter (/= [0]))(tail le) /= [] then Nothing
  else Just []


-- Задача 4.a -----------------------------------------
findRowInt :: Linear -> Int 
findRowInt le = if null le then 1
  else if head(head le)==0 then (findRowInt (tail le))+1
  else 1

findRow :: Linear -> Maybe Int
findRow le = if findRowInt(le) == (length le + 1) then Nothing
  else Just (findRowInt(le))


-- Задача 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow le i = [(le!!(i-1))]++drop 1 (take (i-1) le)++[le!!0]++drop i (le)


-- Задача 5.a -----------------------------------------
results ::  Row -> Row -> Int -> Rational 
results fs xs i = xs!!(length xs-1) - ((xs!!i)/(fs!!i))*fs!!(length xs-1)

elements :: Row -> Row -> Int -> Int -> Row -> Row 
elements fs xs i k res= if (length xs - 1) == k then res++[results(fs) (xs) (0)]
  else elements (fs) (xs) (i) (k+1) (res++[xs!!k-((xs!!i)/(fs!!i))*(fs!!k)])

forwardStep :: Row -> Linear -> Linear
forwardStep fs rs = if null rs then []
  else [elements(fs)(head rs)(0)(1)([])] ++ forwardStep (fs)(tail rs)


-- Задача 5.b -----------------------------------------
multyX :: Row ->  PolinomOne -> PolinomOne
multyX fs rss = if null rss then []
  else if getPolInt(head rss) == 0 then [(getPolInt(head rss),getPolRat(head rss))]++ multyX (fs) (tail rss)
  else [(getPolInt(head rss),(fs!!(getPolInt(head rss)-1)*(getPolRat(head rss))))]++ multyX (fs) (tail rss)

reverseStep2 :: Row -> [PolinomOne] -> PolinomOne
reverseStep2 fs rs = if null rs then [(0,fs!!(length fs - 1))]
  else if filter(<(1,0))(head rs) == [] then coef (-1)(multyX(fs)(head rs)) ++ reverseStep2 (fs)(tail rs)
  else (multyX(fs)(head rs)) ++ reverseStep2 (fs)(tail rs)

reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep fs vs =[coef (1/(fs!!0)) (unify (reverseStep2 fs vs))]++vs


-- Задача 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne]
gauss i le = 
  if null le then 
    Just []
  else if isSimple le then 
    solveSimple le
  else 
    case findRow le of
      Nothing -> 
        let nextLinear = [ [l !! j | j <- [1 .. length l - 1]] | l <- le ]
        in case gauss (i + 1) nextLinear of
             Nothing -> Nothing
             Just pol -> Just $ [(i, 1)] : pol
      Just j -> 
        let exchanged = exchangeRow le j
            forwared = forwardStep (head exchanged) (tail exchanged)
        in case gauss (i + 1) forwared of
             Nothing -> Nothing
             Just nl -> Just $ reverseStep (head exchanged) nl


-- Задача 7.a -----------------------------------------
addAll :: [PolinomOne] -> PolinomOne
addAll pos = foldl add [] pos 

testEquation :: [PolinomOne] -> Row -> Bool 
testEquation pos rs = let result = addAll [coef (rs !! ind) (pos !! ind) | ind <- [0 .. length pos - 1]]
                      in length result == 1 && fst (result !! 0) == 0 && snd (result !! 0) == last rs


-- Задача 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool 
testLinear pos le = foldl (&&) True [testEquation pos l | l <- le]


-- Задача 8 -----------------------------------------
solving :: Linear -> Solution  
solving le = case gauss 1 ([0 | _ <- le !! 0] : le) of
               Just pos -> let polinomsCheck = [length pol == 1 && fst (pol !! 0) == 0 | pol <- pos]
                               singleAnswer = [snd (pol !! 0) | pol <- pos]
                               hasSingleAnswer = foldl (&&) True polinomsCheck 
                           in if not hasSingleAnswer then Many pos 
                                                     else One singleAnswer
               Nothing -> Empty

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne 
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty 
sol2 = Empty 
sol3 = Many res3 
sol4 = One [62/15, -17/15, -4/3] 


