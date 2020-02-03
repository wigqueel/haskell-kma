{-# OPTIONS_GHC -Wall #-}
module Gurin10 where

import Data.Char(isSpace, isDigit, isLetter) 
import Data.Maybe(maybeToList)
type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- ������ 1 -----------------------------------------
spaces :: String -> String                
spaces = dropWhile (isSpace)    

-- ������ 2 ----------------------------------------- 
manyT, value, manyN :: String -> (String,String)   
n:: Char -> Bool
val:: Char -> Bool
t:: Char -> Bool
manyT str = (takeWhile t str, dropWhile t str) 
t str = if (str == '\"') then False else True
  
value str = (takeWhile val str, dropWhile val str)                      
val str = if (str == '>' || str == '<') then False else True

manyN str = (takeWhile n str, dropWhile n str) 
n str = if (isDigit str == True || isLetter str == True || str == '.'|| str == '-')then True else False


-- ������ 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String)                

name line = if (n (f line) == False) then Nothing else Just(manyN (spaces line))           
f line = head(spaces line)                      
           
text line = if (head line /= '<'|| head line /= '>') then Nothing else Just (manyT line)             
             
fullValue [] = Nothing                 
fullValue line = funchelp line "" 0           

{--mod' :: Int -> Int -> Int              
mod' x y | (y > x)   = x
         | otherwise = mod' (x-y) y              
--}
funchelp [] [] _ = Nothing           
funchelp [] _ _ = Nothing
funchelp (x:xs) "" a | a == 1 && x == '\"' = Nothing
                         |a == 0 &&  x == '\"' = funchelp xs "" 1
                         |a == 1  && x /= '\"' = funchelp xs [x] a
                         | otherwise = Nothing 
funchelp (x:xs) line a |a == 1 && x == '\"' = Just (line,xs)                
                          |a == 1  && x /= '\"' = funchelp xs (line++[x]) a                  
                          |a == 0 && x == '\"' = funchelp xs line 1    
                          | otherwise = Nothing 


-- ������ 4 -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 

attrib line = if ((head (dropWhile (isSpace) $ snd $ manyN line))/='=') then Nothing
           else if ((name line)==Nothing) then Nothing 
           else if (fullValue (dropWhile (isSpace) $ drop 1 $ dropWhile (/='=') line))==Nothing then Nothing
           else Just ((fst $ manyN line,func2 $ fullValue (dropWhile (isSpace) $ drop 1 $ dropWhile (/='=') line)),
           func1 $ fullValue (dropWhile (isSpace) $ drop 1 $ dropWhile (/='=') line))

{--ff a = (dropWhile (isSpace) a)--}
manyAtt :: String -> Maybe (Attributes,String) 
manyAtt line = if (null (fff line []) == True) then Nothing else Just (fff line [])

fff a b = if ((attrib a)/=Nothing) then fff (drop 1 $ func1(attrib a)) (concat[b,[func2(attrib a)]]) else (b,a)

-- ������ 5 -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag = undefined

endTag :: String -> Maybe (String,String) 
endTag = undefined

-- ������ 6 -----------------------------------------
element :: String -> Maybe (XML,String) 
element = undefined

xml :: String -> Maybe (XML,String)
xml = undefined

manyXML :: String -> Maybe ([XML],String)
manyXML = undefined

-- ������ 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML  = undefined 
--fullXML  = case element (spaces s) of  
--  Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
--  Nothing      -> Nothing  

-- ������ ���� -------------------------------------------
-- ����� ����� XML-��'���� (��� �������)
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

-- ���������� ������ ���������� XML-��'����
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


func1 :: Maybe (b0,a) -> a
func1 a = snd $ (maybeToList a) !! 0
func2 :: Maybe (a,b0) -> a
func2 a = fst $ (maybeToList a) !! 0