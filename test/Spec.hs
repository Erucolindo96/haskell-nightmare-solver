import Lib

main :: IO ()

test_pix :: [Char]
test_pix =    "0.44."++
              ".4.6."++
              "3.76."++
              ".6.65"++
              "....3"
test_results = "nwnnn"++
               "nnwnn"++
               "nwbnn"++
               "nnnnn"++
               "nnnnn"
               
s_size = 5
lTop = 0 
lBottom = s_size * (s_size - 1)
rTop = s_size - 1
rBottom = s_size^2 - 1

main = do 
  --testy getSquare
  --rogi 
  print(Lib.getSquare lTop s_size test_pix '.'  ==    "....0...4") 
  print(Lib.getSquare lBottom s_size test_pix '.' == "..6......")
  print(Lib.getSquare rTop s_size test_pix '.' ==    "...4..6..")
  print(Lib.getSquare rBottom s_size test_pix '.' == "65..3....")
  --boki
  print(Lib.getSquare 5 s_size test_pix '.' ==  ".0...4.3.") --drugi wiersz, 1 element 
  print(Lib.getSquare 9 s_size test_pix '.' ==  "4..6..6..") --drugi wiersz, ostatni element
  print(Lib.getSquare 2 s_size test_pix '.' ==  "....444.6") --pierwszy wiersz, 3 element
  print(Lib.getSquare 22 s_size test_pix '.' == "6.6......")  --5 wiersz, 3 element
  --srodek
  print(Lib.getSquare 16 s_size test_pix '.' == "3.7.6....")  -- 4 wiersz, 2 element

  --metoda fill - zamaluj lewy górny 2x2
  print(Lib.fill 'b' 0 [0, 1, s_size, s_size+1] test_results  ==  "bwnnn"++
                                                                         "bbwnn"++
                                                                         "nwbnn"++
                                                                         "nnnnn"++
                                                                         "nnnnn")

  --metoda fill - zamaluj lewy dolny 2x2
  print(Lib.fill 'b' 0 [s_size*3, s_size*3 + 1, 
                        s_size*4, s_size*4 + 1] test_results  ==  "nwnnn"++
                                                                  "nnwnn"++
                                                                  "nwbnn"++
                                                                  "bbnnn"++
                                                                  "bbnnn")
  --metoda fill - zamaluj prawy górny 2x2
  print(Lib.fill 'b' 0 [3, 4, s_size + 3, s_size + 4] test_results  ==  
                                                                   "nwnbb"++
                                                                   "nnwbb"++
                                                                   "nwbnn"++
                                                                   "nnnnn"++
                                                                   "nnnnn")
  --metoda fill - zamaluj prawy dolny 2x2
  print(Lib.fill 'b' 0 [s_size*3 + 3, s_size*3 + 4, 
                        s_size*4 + 3, s_size*4 + 4] test_results  ==  
                                                                  "nwnnn"++
                                                                  "nnwnn"++
                                                                  "nwbnn"++
                                                                  "nnnbb"++
                                                                  "nnnbb")

  --metoda fill - zamaluj srodek 3x3
  print(Lib.fill 'w' 0 [s_size + 1, s_size + 2, s_size + 3,
                               s_size*2 + 1, s_size*2 + 2, s_size*2 + 3,
                               s_size*3 + 1, s_size*3 + 2, s_size*3 + 3]
                               test_results  ==  "nwnnn"++
                                                 "nwwwn"++
                                                 "nwbwn"++
                                                 "nwwwn"++
                                                 "nnnnn")

  -- metoda getSquareIndexes - zwroc prawy górny 2x2
  print(Lib.returnSquareIndexes 4 s_size == [3,4, s_size+3, s_size+4])

  -- metoda getSquareIndexes - zwroc lewy górny 2x2
  print(Lib.returnSquareIndexes 0 s_size == [0,1, s_size, s_size+1])
  
  -- metoda getSquareIndexes - zwroc prawy dolny 2x2
  print(Lib.returnSquareIndexes (s_size^2 - 1) s_size == [s_size*3+3, s_size*3 +4, s_size*4 + 3, s_size*4 + 4])
  
  -- metoda getSquareIndexes - zwroc lewy dolny 2x2
  print(Lib.returnSquareIndexes (s_size*4) s_size == [s_size*3, s_size*3 + 1, s_size*4, s_size*4 + 1])
  
  -- metoda getSquareIndexes - zwroc dolny  2x3
  print(Lib.returnSquareIndexes (s_size*4 + 2) s_size == [s_size*3 + 1,s_size*3 + 2, s_size*3 + 3,
                                                            s_size*4 + 1,s_size*4 + 2, s_size*4 + 3
                                                           ])
  -- metoda getSquareIndexes - zwroc górny  2x3
  print(Lib.returnSquareIndexes 2 s_size == [1, 2, 3,
                                            s_size + 1, s_size + 2, s_size + 3
                                            ])
                                            
  -- metoda getSquareIndexes - zwroc lewy  3x2
  print(Lib.returnSquareIndexes s_size s_size == [0 ,1, 
                                                  s_size, s_size + 1, 
                                                  s_size*2, s_size*2 + 1
                                                  ])
-- metoda getSquareIndexes - zwroc prawy  2x3
  print(Lib.returnSquareIndexes (s_size + 4) s_size == 
                                                  [3 , 4, 
                                                  s_size + 3, s_size + 4, 
                                                  s_size*2 + 3, s_size*2 + 4
                                                  ])
  -- metoda getSquareIndexes - zwroc lewy gorno-diagonalny 3x3
  print(Lib.returnSquareIndexes (s_size + 1) s_size == 
                                                  [0 , 1, 2 ,
                                                  s_size, s_size + 1, s_size + 2, 
                                                  s_size*2, s_size*2 + 1, s_size*2 + 2
                                                  ])