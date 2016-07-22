module ChapterExamples where

import Control.Applicative
import Data.Char
--hurr :: Num a => a -> a
hurr = (*2)
--durr :: Num a => a -> a
durr = (+10)

m :: Integer -> Integer
m = hurr . durr


m2 :: Integer -> Integer
m2 = (+)  <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped  = cap <$> rev


tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,)  rev cap

tupled' :: [Char] -> ([Char], [Char])
tupled' =
  do
    a <- rev
    b <- cap
    return (a,b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= \a -> cap >>= \b -> return (a,b)

tupled''' = (,) <$> cap <*> rev
