module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

-- Write a program that prints the numbers from
-- 100. But for multiples of three print "Fizz"
-- instead of the number and for the multiples of
-- five print "Buzz". For numbers which are multiples of both
-- three and five print "FizzBuzz"

fizzBuzz ::  Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =

  let y = traverse addResult list
  in execState y []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer ->  [String] 
fizzbuzzFromTo fm to = flip execState []$  sequenceA $ foldl f [] [fm..to]
  where
    f :: [State  [String] ()] -> Integer -> [State [String] ()]
    f b a = addResult a : b

main :: IO()
main =
  mapM_ putStrLn $ reverse$ fizzbuzzList [1..100]
