import Lib

main :: IO ()

test_pix :: [Char]
test_pix =    "0.44."++
              ".4.6."++
              "3.76."++
              ".6.65"++
              "....3"
s_size = 5
  
lTop = 0 
lBottom = s_size * (s_size - 1)
rTop = s_size - 1
rBottom = s_size^2 - 1

--getSquare i s_size pix
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
    
      