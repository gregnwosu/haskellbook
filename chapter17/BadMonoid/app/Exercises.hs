module Exercises where

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
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil Nil = Nil
  mappend xs Nil = xs
  mappend Nil ys = ys
  mappend (Cons x xs) l = Cons x (xs `mappend` l)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) ((pure f <*> xs) <> (fs <*> pure x))


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = foldr Cons Nil <$> listOf arbitrary

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

newtype ZipList' a = ZipList' (List a)
  deriving (Eq,Show)


instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where xs' = let (ZipList' l ) = xs
                in take' 3000 l

         ys' = let (ZipList' l) = ys
                in take' 3000 l
main :: IO()
main = quickBatch $ applicative (Nil :: List (Char,Char,Char ))
