module Main where

import Criterion.Main

infixl 9 !?!
infixl 9 !?

_  !?! n | n < 0 = Nothing
[] !?! _ = Nothing
(x:_)  !?! 0 = Just x
(_:xs) !?! n = xs !?! (n-1)

{-# INLINABLE (!?) #-}

--TODO: how the actual fuck do em get away with passing a 3 arity function to foldr?!?!?!

z :: undefined -> Maybe a
z = const Nothing

(!?) :: [a] -> Int -> Maybe a
xs !? n
   | n < 0 = Nothing
   | otherwise = foldr f z xs n
                 where
                   f :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
                   f a _ 0 = Just a
                   f _ b n = b (n-1)

myList :: [Int]
myList = [1..9999]

main :: IO()
main = defaultMain [
 bench "index list 9999" $
       whnf (myList !!) 9998,
 bench "index list maybe index 9999" $
       whnf (myList !?!) 9998,
 bench "index list maybe index 9999" $
       whnf (myList !?) 9998]
