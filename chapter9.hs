module Chapter9 where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool = enumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = enumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = enumFromTo

eftChar :: Char -> Char -> String
eftChar = enumFromTo

myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (,)

myZipWith :: (a -> b  -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys)  = f x y : myZipWith f xs ys

removeLower :: String -> String
removeLower  = filter isUpper

upcaseInitial :: String -> String
upcaseInitial [] = []
upcaseInitial (x:xs) = toUpper x:xs

myupcase :: String -> String
myupcase [] = []
myupcase (x:xs) = toUpper x : myupcase xs

upcaseFirst :: String -> Char
upcaseFirst = toUpper . head

myAnd  :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =  not x  ||  myAnd xs

myOr  :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =  x  ||  ( myOr xs)


myany  :: (a -> Bool) -> [a] -> Bool
myany _ [] = False
myany f (x:xs) = f x || myany f xs


myelem :: Eq a => a -> [a] -> Bool
myelem a = myany (==a) 


myreverse :: [a] -> [a]
myreverse = go [] 
  where
    go :: [a] -> [a] ->[a]
    go a [] = a
    go a (x:xs) =  go (x:a) xs

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []  = []
squishMap f (x:xs) = f x ++ squishMap f xs 
