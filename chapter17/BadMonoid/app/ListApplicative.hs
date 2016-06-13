module ListApplicative () where

import Data.List (elemIndex)
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil |
  Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f  = concat' . fmap f

instance Applicative List where
  pure = (`Cons` Nil)
  fs <*> xs = flatMap (\f -> fold (go f) Nil  xs) fs
     where
       go  :: (x-> y) -> x -> List y -> List y
       go f x z = Cons (f x) z

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = foldr Cons Nil <$> listOf arbitrary

take' :: Int -> List a -> List a
take' 0 l = l
take' n (Cons _ t) = take' (subtract n 1) t

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
     where xs' = take' 3000 xs
           ys' = take' 3000 ys

n :: List (Char,Char,Char)
n = Nil

main :: IO()
main = quickBatch $ applicative n
