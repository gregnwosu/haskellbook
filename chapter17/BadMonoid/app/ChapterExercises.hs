module ChapterExercises where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ListPrime a = ListPrime [a]
  deriving (Show, Eq)

newtype MyFunc x y = MyFunc (x -> y)
  deriving (Show)

instance Functor (MyFunc x) where
  fmap f (MyFunc f') = MyFunc $ f . f'

instance Applicative (MyFunc x) where
  pure = MyFunc . const
  (MyFunc f) <*> (MyFunc x) = MyFunc $ f <*> x

instance Functor ListPrime where
  fmap f (ListPrime l) = ListPrime (fmap f l)

instance Monoid (ListPrime a) where
  mempty = ListPrime []
  (ListPrime []) `mappend` (ListPrime []) = ListPrime []
  (ListPrime []) `mappend` g = g
  f `mappend` (ListPrime []) = f
  (ListPrime f)  `mappend` (ListPrime g) = ListPrime $ f <> g

instance Applicative (ListPrime) where
  pure x   = ListPrime [x]
  (ListPrime fs) <*> (ListPrime xs) = ListPrime $ fs <*> xs

instance Arbitrary a => Arbitrary (ListPrime a) where
  arbitrary = ListPrime  <$> listOf arbitrary

instance Arbitrary a => Arbitrary (MyIO  a) where
  arbitrary = (MyIO . return)  <$>  arbitrary

newtype MyIO a = MyIO (IO a)

newtype MyTuple a b = MyTuple (a,b)
  deriving (Eq, Show)

instance Functor (MyTuple a) where
  fmap f (MyTuple (a,x)) = MyTuple  (a, f x)

instance Monoid a => Applicative (MyTuple a) where
  pure x = MyTuple (mempty, x)
  (MyTuple (a,f)) <*> (MyTuple (a',x)) = MyTuple (a <> a', f x)



instance Show (MyIO a) where
  show _ = "boo"

instance Functor MyIO where
  fmap f (MyIO ioa) = MyIO $ fmap f ioa

instance Applicative MyIO where
  pure x = MyIO $ return x
  (MyIO f) <*> (MyIO g) = MyIO $ f <*> g

n :: ListPrime (Char,Char,Char)
n = mempty

i :: MyIO (Char,Char,Char)
i = undefined

instance Eq a => EqProp (ListPrime a) where
  (=-=) = eq

test :: IO()
test = quickBatch $ applicative n
