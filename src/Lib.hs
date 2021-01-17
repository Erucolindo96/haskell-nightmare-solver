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
exampleStr =  "....1"++
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

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list
                    
--someFunc = print (getSquare rBottom s resultStr 'b')
--someFunc = print (solve exampleStr 0 25 5 resultStr)

result1 = splitEvery 5 (solve exampleStr 0 25 5 resultStr)

someFunc= do 
  print(result1!!0)
  print(result1!!1)
  print(result1!!2)
  print(result1!!3)
  print(result1!!4)


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
 
-- i - indeks elementu bedacego srodkiem kwadratu 3x3
-- s_size - długośc boku planszy
returnSquareIndexes :: Int -> Int -> [Int]
returnSquareIndexes i s_size
  | i == leftTop = --jestes w lewym górnym rogu
    [i, i+1, i+s_size, i+s_size+1]
  | i == rightTop = --jestes w prawym gornym rogu
    [i-1, i, i+s_size-1,i+s_size]
  | i == leftBottom = -- jestes w lewym dolnym  
    [i-s_size, i-s_size + 1, i, i+1]
  | i == rightBottom = --jestes w prawym dolnym rogu
    [ i-s_size-1, i-s_size ,i-1, i]
  | i > leftTop && i < rightTop = --jestes w gornej czesci
    [i-1, i, i+1,i+s_size-1, i+s_size, i+s_size+1]
  | i > leftBottom && i < rightBottom = --jestes w dolnej czesci
    [ i-s_size-1, i-s_size, i-s_size+1, i-1, i, i+1]
  | i `mod` s_size == 0 = --jestes w lewej czesci
    [i-s_size, i-s_size+1, i, i+1, i+s_size, i +s_size+1]
  | i `mod` s_size == s_size - 1 =  -- jestes w prawej czesci
    [i-s_size-1, i-s_size, i-1, i, i+s_size-1, i+s_size]
  | otherwise =  --jestes w srodku macierzy
    [i-s_size-1, i-s_size, i-s_size+1, i-1, i, i+1, i+s_size-1, i+s_size, i+s_size+1]
  where
    leftTop = 0 
    leftBottom = s_size * (s_size - 1)
    rightTop = s_size - 1
    rightBottom = s_size^2 - 1

-- c - znak ktory wstawiamy
-- i - indeks aktualnie obrabianego punktu w przekazanej liście(planszy)
-- indexes - indeksy listy (r:restResults) które mamy spróbować zamalować(zamienić 'n' na wskazany jako c znak)
-- (r:restResults) - lista której elementy malujemy (plansza resultatów)
-- 
fill :: Char -> Int -> [Int] -> [Char] -> [Char]    
fill _ _ _ [] = []    
fill c i indexes (r:restResults)
 | i `elem` indexes && r == 'n' = c:fill c (i+1) indexes restResults
 | otherwise = r:fill c (i+1) indexes restResults


     
-- dostajesz kwadrat 3x3 i result - sprobuj go pokolorować  
-- i - srodek kwadratu 3x3 który próbujemy pomalować
-- s_size - długośc boku planszy
-- pix - plansza z cyframi
-- square_result - kwadrat 3x3 o srodku w i (wycinek listy stanu pokolorować - wycinek result)
-- result - lista będaca stanem pokolorowania planszy
processOneSquare :: Int -> Int -> [Char] -> [Char] -> [Char] -> [Char]
processOneSquare i s_size pix square_result result 
 | field_num == 0 = --zamaluj wszystko na bialo
  fill 'w' 0 (returnSquareIndexes i s_size) result
 | field_num == 9 = --zamaluj wszystko na czarno
  fill 'b' 0 (returnSquareIndexes i s_size) result
 | field_num == b_cnt = -- zamaluj wszystkie ny na biało
  fill 'w' 0 (returnSquareIndexes i s_size) result
 | b_n_sum == field_num = --jest tyle pól niepokolorowanych co liczba w srodku kwadratu
  fill 'b' 0 (returnSquareIndexes i s_size) result
 | otherwise = result
 where 
  field_num = digitToInt (pix!!i) 
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

-- wywołuje próbe pokolorowania dla każdego znaczączego elementu planszy
-- leci po kolei przez plansze, próbuje kolorować
-- i - indeks dla którego zaczynamy rozwiązywanie 
-- n - dlugosc listy (planszy)
-- s_size - długość boku planszy
-- pix - plansza 
-- result -- obecny stan pokolorowania planszy (kwadrat s_size x s_size wypełniany podczas rozwiązywania)
solver :: Int -> Int -> Int -> [Char] -> [Char] -> [Char]
solver i n s_size pix result
  | i >= n = result
  | pix!!i == '.' = solver (i+1) n s_size pix result
  | otherwise = solver (i+1) n s_size pix (processOneSquare i s_size pix (getSquare i s_size result 'w') result)

-- Wywołuje w każdej iteracji próbę pokolorowania dla wszystkich kwadratów 3x3 na planszy (ze znaczących pól)
-- pix - plansza
-- iter - obecna iteracja 
-- n - ilosc iteracji
-- s_size = dlugosc boku planszy
-- result - obecny stan pokolorowania
solve :: [Char] -> Int -> Int -> Int -> [Char] -> [Char]
solve pix iter n s_size result = if iter >= n 
                          then result 
                            else solve pix (iter+1) n s_size (solver 0 (s_size^2) s_size pix result)

