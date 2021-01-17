module Lib
    ( someFunc, getSquare, fill, returnSquareIndexes
    ) where

import Data.Char (digitToInt)


s :: Int
s = 5
lTop :: Integer
lTop = 0 
lBottom :: Int
lBottom = s * (s - 1)
rTop :: Int
rTop = s -1 
rBottom :: Int
rBottom = s^2 - 1

  
exampleStr :: [Char]
exampleStr =  "...1"++
              ".9..."++
              ".88.."++
              "....4"++
              "4.5.2"
resultStr :: [Char]
resultStr  = "nnnnn"++
             "nnnnn"++
             "nnnnn"++
             "nnnnn"++
             "nnnnn"

someFunc :: IO ()
                    
--someFunc = print (getSquare rBottom s resultStr 'b')
someFunc = print (solve exampleStr 0 25 resultStr)

example :: [[Char]]
example = 
  [
  "0.44.", 
  ".4.6.",
  "3.76.", 
  ".6.65", 
  "....3"]
  
square_size :: Int
square_size = 5  
--local_pix =   "..."++
--              ".0."++
--              "..4"
--              
--local_result ="bbb"++
--              "bnn"++
--              "bnn"
 
 --zliczanie elementów w tablicy
 -- a - element zliczany
 -- [a] - tablica w której zliczamy element
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==) 
 

returnSquareIndexes :: Int -> Int -> [Int]
returnSquareIndexes i s_size
  | i == leftTop = --jestes w lewym górnym rogu
    [i, i+1, i+s_size, i+(s_size+1)]
  | i == rightTop = --jestes w prawym gornym rogu
    [i-1, i, i+(s_size-1),i+s_size]
  | i == leftBottom = -- jestes w lewym dolnym  
    [i-s_size, i-s_size + 1, i, i+1]
  | i == rightBottom = --jestes w prawym dolnym rogu
    [ i-s_size-1, i-s_size ,i-1, i]
  | i > leftTop && i < rightTop = --jestes w gornej czesci
    [i-1, i, i+1,i+(s_size-1), i+s_size, i+(s_size+1)]
  | i > leftBottom && i < rightBottom = --jestes w dolnej czesci
    [ i-s_size-1, i-s_size, i-s_size+1, i-1, i, i+1]
  | i `mod` s_size == 0 = --jestes w lewej czesci
    [i-s_size, i-s_size+1, i, i+1, i+s_size, i +(s_size+1)]
  | i `mod` s_size == s_size - 1 =  -- jestes w prawej czesci
    [i-s_size-1, i-s_size, i-1, i, i+(s_size-1), i+s_size]
  | otherwise =  --jestes w srodku macierzy
    [i-s_size-1, i-s_size, i-s_size+1, i-1, i, i+1, i+(s_size-1), i+s_size, i+(s_size+1)]
  where
    leftTop = 0 
    leftBottom = s_size * (s_size - 1)
    rightTop = s_size - 1
    rightBottom = s_size^2 - 1

fill :: Char -> Int -> Int -> [Int] -> [Char] -> [Char]    
fill _ _ _ _ [] = []    
fill c i size indexes (r:restResults)
 | i `elem` indexes && r == 'n' = c:fill c (i+1) size indexes restResults
 | otherwise = r:fill c (i+1) size indexes restResults


     
-- dostajesz kwadrat 3x3 i result - sprobuj go pokolorować  
processOneSquare :: Int -> Int -> [Char] -> [Char] -> [Char] -> [Char]
processOneSquare i s_size square square_result result 
 | field_num == 0 = --zamaluj wszystko na bialo
  fill 'w' i s_size (returnSquareIndexes i s_size) result
 | field_num == 9 = --zamaluj wszystko na czarno
  fill 'b' i s_size (returnSquareIndexes i s_size) result
 | field_num == b_cnt = -- zamaluj wszystkie ny na biało
  fill 'w' i s_size (returnSquareIndexes i s_size) result
 | b_n_sum == field_num = --jest tyle pól niepokolorowanych co liczba w srodku kwadratu
  fill 'b' i s_size (returnSquareIndexes i s_size) result
 | otherwise = result
 where 
  field_num = digitToInt (square!!4) 
  n_cnt = count 'n' square_result
  b_cnt = count 'b' square_result
--  w_cnt = count 'w' square_result
  b_n_sum = n_cnt + b_cnt
  
 
 
-- zwraca kwadrat 3x3 z plamnszy pix którego środek znajduje się w i. Uzupełnia 
-- i - srodek kwadratu
-- s_size - długośc boku całej planszy
-- pix - plansza
-- c - znak który wstawiamy jako "obramowanie" planszy
getSquare :: Int -> Int -> [Char] -> Char -> [Char]
getSquare i s_size pix c
  | i == leftTop = --jestes w lewym górnym rogu
    [
     c, c, c,
     c, pix!!i, pix!!(i+1),
     c, pix!!(i+s_size), pix!!(i+(s_size+1))
    ]
  | i == rightTop = --jestes w prawym gornym rogu
      [
      c, c, c,
      pix!!(i-1), pix!!i, c,
      pix!!(i+(s_size-1)), pix!!(i+s_size), c
      ]
  | i == leftBottom = -- jestes w lewym dolnym  
    [
      c, pix!!(i-s_size), pix!!(i-s_size + 1),
      c,  pix!!i, pix!!(i+1),
      c, c, c
    ]
  | i == rightBottom = --jestes w prawym dolnym rogu
    [
    pix!!(i-s_size-1), pix!!(i-s_size), c,
    pix!!(i-1), pix!!i, c,
    c, c, c
    ]
  | i > leftTop && i < rightTop = --jestes w gornej czesci
    [
      c, c, c,
      pix!!(i-1), pix!!i, pix!!(i+1), 
      pix!!(i+(s_size-1)), pix!!(i+s_size), pix!!(i+(s_size+1))
    ]
  | i > leftBottom && i < rightBottom = --jestes w dolnej czesci
    [
      pix!!(i-s_size-1), pix!!(i-s_size), pix!!(i-s_size+1),
      pix!!(i-1), pix!!i, pix!!(i+1),
      c, c, c
    ]
  | i `mod` s_size == 0 = --jestes w lewej czesci
    [
    c, pix!!(i-s_size), pix!!(i-s_size+1),
    c, pix!!i, pix!!(i+1),
    c, pix!!(i+s_size), pix!!(i+(s_size+1))
    ]
  | i `mod` s_size == s_size - 1 =  -- jestes w prawej czesci
    [
    pix!!(i-s_size-1), pix!!(i-s_size), c,
    pix!!(i-1), pix!!i, c,
    pix!!(i+(s_size-1)), pix!!(i+s_size), c
    ]
  | otherwise = [ --jestes w srodku macierzy
      pix!!(i-s_size-1), pix!!(i-s_size), pix!!(i-s_size+1),
      pix!!(i-1), pix!!i, pix!!(i+1), 
      pix!!(i+(s_size-1)), pix!!(i+s_size), pix!!(i+(s_size+1))
      ]
  where
    leftTop = 0 
    leftBottom = s_size * (s_size - 1)
    rightTop = s_size - 1
    rightBottom = s_size^2 - 1

--processOneSquare i s_size square square_result result 

--proba rozwiazania zaczynając od elementu x
solver :: Int -> Int -> [Char] -> [Char] -> [Char]
solver i n pix result
  | i >= n = result
  | pix!!i == '.' = solver (i+1) n pix result
  | otherwise = solver (i+1) n pix (processOneSquare i n (getSquare i square_size pix '.') (getSquare i square_size result 'b') result)

--glowna fcja programu - bierze "example", wypluwa "result"
solve :: [Char] -> Int -> Int -> [Char] -> [Char]
solve pix i n result = if i >= n 
                          then result 
                            else solve pix (i+1) n (solver 0 n pix result)
--solve _  result = result


--takeSquare :: (pix -> i -> square) -> [pix] -> i -> [square]

--["0.44.", ".4.6.", "3.76.", ".6.65", "....3"]

--result = ["c","b", "n"]
