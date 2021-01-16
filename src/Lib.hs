module Lib
    ( someFunc, getSquare
    ) where

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
exampleStr =  "0.44."++
              ".4.6."++
              "3.76."++
              ".6.65"++
              "....3"
resultStr :: [Char]
resultStr  = "nnnnn"++
             "nnnnn"++
             "nnnnn"++
             "nnnnn"++
             "nnnnn"

someFunc :: IO ()
--someFunc = print (solve exampleStr 0 25 resultStr)
someFunc = print (getSquare rBottom s resultStr 'b')
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
 
-- dostajesz kwadrat 3x3 i result - sprobuj go pokolorować  
processOneSquare square square_result result = result
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



--proba rozwiazania zaczynając od elementu x
solver :: Int -> Int -> [Char] -> [Char] -> [Char]
solver i n pix result
  | i >= n = result
  | pix!!i == '.' = solver (i+1) n pix result
  | otherwise = solver (i+1) n pix (processOneSquare (getSquare i square_size pix '.') (getSquare i square_size result 'b') result)

--glowna fcja programu - bierze "example", wypluwa "result"
solve :: [Char] -> Int -> Int -> [Char] -> [Char]
solve pix i n result = if i >= n 
                          then result 
                            else solve pix (i+1) n (solver i n pix result)
--solve _  result = result


--takeSquare :: (pix -> i -> square) -> [pix] -> i -> [square]

--["0.44.", ".4.6.", "3.76.", ".6.65", "....3"]

--result = ["c","b", "n"]
