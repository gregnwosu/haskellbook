module Main where

incdInts :: [Integer] -> [Integer]
incdInts = map (+1)

main :: IO ()
main = print $ incdInts [1..] !! 1000


-- toDO compare pointfree heapprofiles for incdInts, as per page 1093 of haskellbook, pointfree functions results are shared on the heap so we expect a space leak of sorts when mapping over the infinite list [1..]
