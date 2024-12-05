{-# OPTIONS_GHC -Wall #-}
module Antoshchuk06 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces = dropWhile isSpace

-- Задача 2.a ----------------------------------------- 
manyT :: String -> (String,String)
manyT s = span (/= '<') s

-- Задача 2.b ----------------------------------------- 
value :: String -> (String,String)
value s = span (/= '\"') s

-- Задача 2.c ----------------------------------------- 
manyN :: String -> (String,String)
manyN s = span (\c -> isLetter c || isDigit c) s

-- Задача 3.a -----------------------------------------
name :: String ->  Maybe(String,String) 
name s = let (nm, rest) = manyN s
         in if not (null nm) then Just (nm, rest) else Nothing

-- Задача 3.b -----------------------------------------
text :: String ->  Maybe(String,String) 
text s = let (txt, rest) = manyT s
         in if not (null txt) then Just (txt, rest) else Nothing

-- Задача 3.c -----------------------------------------
fullValue :: String ->  Maybe(String,String) 
fullValue ('\"':s) = let (val, rest) = value s
                     in if not (null val) && not (null rest) && head rest == '\"'
                        then Just (val, tail rest)
                        else Nothing
fullValue _ = Nothing

-- Задача 4.a -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 
attrib str = do
    let str' = dropWhile isSpace str
    let (name, rest1) = span (/= '=') str'
    if null rest1 || head rest1 /= '='
    then Nothing
    else do
        let rest2 = drop 1 rest1
        let rest3 = dropWhile isSpace rest2
        if not (null rest3) && head rest3 == '\"'
        then do
            let (value, rest4) = span (/= '\"') (tail rest3)
            if null rest4
            then Nothing
            else return ((name, value), tail rest4)
        else Nothing

-- Задача 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String) 
manyAtt str = do
    let str' = dropWhile isSpace str
    if null str' || head str' == '>'
    then Just ([], str')
    else do
        (att, rest) <- attrib str'
        (atts, rest') <- manyAtt rest
        return (att : atts, rest')

-- Задача 5.a -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag str = do
    let str' = dropWhile isSpace str
    if head str' == '<'
    then do
        let (name, rest1) = span (/= ' ') (tail str')
        (atts, rest2) <- manyAtt rest1
        if head rest2 == '>'
        then return ((name, atts), tail rest2)
        else Nothing
    else Nothing

-- Задача 5.b -----------------------------------------
endTag :: String -> Maybe (String,String) 
endTag str = do
    let str' = dropWhile isSpace str
    if take 2 str' == "</"
    then do
        let (name, rest1) = span (/= '>') (drop 2 str')
        return (name, tail rest1)
    else Nothing

-- Задача 6.a -----------------------------------------
element :: String -> Maybe (XML,String) 
element str = do
    ((name, atts), rest1) <- begTag str
    (children, rest2) <- manyXML rest1
    (endName, rest3) <- endTag rest2
    if name == endName
       then return (Element name atts children, rest3)
       else Nothing  

-- Задача 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml str = element str

-- Задача 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML str = do
    let str' = dropWhile isSpace str
    if null str'
    then Just ([], str')
    else do
        (el, rest1) <- element str'
        (els, rest2) <- manyXML rest1
        return (el : els, rest2)

-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML str = do
    (xmlEl, rest) <- xml str
    if null (dropWhile isSpace rest)
    then return xmlEl
    else Nothing

-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



