module PoemLines where

myWords :: String -> [String]

myWords [] = []
myWords x =  (dw s . tw )  x : myWords (dw s . dw ns $ x )
  where s = (==' ' )
        ns = not . s
        dw = dropWhile
        tw = takeWhile ns
