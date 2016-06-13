module ZipListApplicative () where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil Nil = Nil
  mappend xs Nil  = xs
  mappend Nil ys  = ys
  mappend (Cons x xs) l = Cons x (xs `mappend` l)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = (`Cons` Nil)
  (Cons f fs) <*> (Cons x xs) = Cons (f x) ((pure f <*> xs) <> (fs <*> pure x))

take' :: Int -> List a -> List a
take' 0 l = l
take' _ Nil = Nil
take' n (Cons _ t) = take' (subtract n 1) t

newtype ZipList' a =
  ZipList'(List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
     where xs' = let (ZipList' l) = xs
                 in take' 3000 l
           ys' = let (ZipList' l) = ys
                 in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' l) = ZipList' $  fmap f l

append :: List a -> List a -> List a
append Nil ys = ys
append x Nil = x
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f  = concat' . fmap f

repeat' :: a -> List a
repeat' x =  Cons x (repeat' x)

zipRepeat :: a -> ZipList' a
zipRepeat z = ZipList' $ repeat' z

(<+>) :: Monoid a  => List a -> List a -> List a
Nil <+> x = x
x <+> Nil = x
Cons x xs <+> Cons y ys = Cons (x <> y) (xs <+> ys)

instance Monoid (ZipList' a) where
  mempty = ZipList' Nil
  a `mappend` (ZipList' Nil) = a
  (ZipList' Nil) `mappend` a = a
  (ZipList' f) `mappend` (ZipList' g) = ZipList' $ f <> g


instance Applicative ZipList' where
   pure x= ZipList' (Cons x Nil)
   _ <*> ZipList' Nil = ZipList' Nil
   ZipList' Nil <*> _ = ZipList' Nil
   ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
     ZipList' (Cons (f x) Nil) <> (pure f  <*> ZipList' xs) <> (ZipList' fs <*> pure x)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' . foldr Cons Nil <$> listOf arbitrary

n :: ZipList' (Char,Char,Char )
n = ZipList' Nil


test :: IO()
test = quickBatch $ applicative n
