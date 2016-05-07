module WordNumber where

import Data.List

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"


digits :: Int -> [Int]
digits= go []
       where
         go::[Int] -> Int -> [Int]
         go l 0 = l
         go l n = go  (n `mod`10 :l) (n `div` 10)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
