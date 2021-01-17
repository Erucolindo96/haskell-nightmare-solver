module Lib
    (printResults, getSquare, fill, returnSquareIndexes, splitEvery, solver, solve
    ) where

import Data.Char (digitToInt)
import System.IO (readFile)


printResults :: IO ()
printResults = do 
  content <- readFile "/home/krzysztof/IdeaProjects/prywata/fill-a-pix/data/basic/20-na-20"
  let pix = read content ::[String]         --wczytana plansza
  let s_size = length pix                   --długość boku planszy
  let pixAsString = concat pix              --plansza jako pojedynczy string
  let resultStr = replicate (s_size^2) 'n'  --stan pokolorowania planszy
  let results = splitEvery s_size (solve pixAsString 0 (s_size^2) s_size resultStr)
  mapM_ print results

-- Dzieli liste na liste list. 
-- Podział następuje co n elementów
-- n - co ile elementów lista jest dzielona
-- list - lista która dzielimy  
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list


 -- zliczanie elementów w tablicy
 -- a - element zliczany
 -- [a] - tablica w której zliczamy element
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==) 
 
 -- Zwraca indeksy elementów planszy, które znajdują się w kwadracie 3x3 o środku w i
 -- Konieczne do zamalowywania obszaru, aby próbowac zamalowywać tylko elementy planszy, a nie sąsiednie
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

-- Zamalowuje kwadrat 3x3 o środku w i na wskazany kolor c
-- c - znak ktory wstawiamy w miejsce 'n' (kolor. 'b' - black, 'w' - while)
-- i - indeks aktualnie obrabianego punktu w przekazanej liście(planszy)
-- indexes - indeksy listy (r:restResults) które mamy spróbować zamalować(zamienić 'n' na wskazany jako c znak)
-- (r:restResults) - lista której elementy malujemy (plansza resultatów)
-- 
fill :: Char -> Int -> [Int] -> [Char] -> [Char]    
fill _ _ _ [] = []    
fill c i indexes (r:restResults)
 | i `elem` indexes && r == 'n' = c:fill c (i+1) indexes restResults
 | otherwise = r:fill c (i+1) indexes restResults

     
-- próba zamalowania kwadratu 3x3 o środku w i 
-- sprawdza po kolei, czy da sie zastosować któraś z reguł kolorowania planszy
-- jeśli tak, przetwarza stan pokolorowania "result" tak, aby zamalować elementy "niewiadome" 'n' na odpowiedni kolor
-- zwraca stan pokolorowania planszy po przetworzeniu obecnego kwadratu 3x3   
-- i - srodek kwadratu 3x3 który próbujemy pomalować
-- s_size - długośc boku planszy
-- pix - plansza z cyframi
-- square_result - kwadrat 3x3 o srodku w i (wycinek listy stanu pokolorować - wycinek result)
-- result - lista będaca stanem pokolorowania planszy
processOneSquare :: Int -> Int -> [Char] -> [Char] -> [Char] -> [Char]
processOneSquare i s_size pix square_result result 
 | field_num == 0 = -- zamaluj wszystko na bialo
  fill 'w' 0 (returnSquareIndexes i s_size) result
 | field_num == 9 = -- zamaluj wszystko na czarno
  fill 'b' 0 (returnSquareIndexes i s_size) result
 | field_num == b_cnt = -- zamaluj wszystkie ny na biało
  fill 'w' 0 (returnSquareIndexes i s_size) result
 | b_n_sum == field_num = -- jest tyle pól niepokolorowanych co liczba w srodku kwadratu
  fill 'b' 0 (returnSquareIndexes i s_size) result
 | otherwise = result
 where 
  field_num = digitToInt (pix!!i) 
  n_cnt = count 'n' square_result
  b_cnt = count 'b' square_result
  b_n_sum = n_cnt + b_cnt
  

-- zwraca kwadrat 3x3 z planszy pix którego środek znajduje się w i. 
-- Jeśli srodek kwadratu znajduje sie na brzegu planszy - uzupełnia kwadrat znakiem c 
-- Potrzebne w celu pobierania kwadratów 3x3 z listy opisującej plansze, oraz z listy opisująćek stan pokolorowania planszy 
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

-- wywołuje próbe pokolorowania dla każdego znaczączego elementu(zawierająćego numere) planszy
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
-- jesli ilosc iteracji jest równa ilości pól planszy - powinien rozwiązać każda zagadkę o prostych zasadach
-- pix - plansza
-- iter - obecna iteracja 
-- n - ilosc iteracji
-- s_size = dlugosc boku planszy
-- result - obecny stan pokolorowania. Początkowo lista długości s_size^2 zawierająca same litery 'n'(nulle - kolor nieznany)
solve :: [Char] -> Int -> Int -> Int -> [Char] -> [Char]
solve pix iter n s_size result = if iter >= n 
                          then result 
                            else solve pix (iter+1) n s_size (solver 0 (s_size^2) s_size pix result)

