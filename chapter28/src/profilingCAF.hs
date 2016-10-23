module Main where

incdInt :: [Integer]
incdInt = map (+1) [1..]

main :: IO ()
main = do
  print (incdInt !! 1000)
  print (incdInt !! 9001)
  print (incdInt !! 90010)
  print (incdInt !! 9001000)
  print (incdInt !! 9501000)
  print (incdInt !! 9901000)
