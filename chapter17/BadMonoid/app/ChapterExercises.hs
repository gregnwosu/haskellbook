module ChapterExercises where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Show (MyIO a) where
  show _ = "boo"

instance Monoid (ListPrime a) where
  mempty = ListPrime []
  (ListPrime []) `mappend` (ListPrime []) = ListPrime []
  (ListPrime []) `mappend` g = g
  f `mappend` (ListPrime []) = f
  (ListPrime f)  `mappend` (ListPrime g) = ListPrime $ f <> g

instance Monoid a => Applicative (MyTuple a) where
  pure x = MyTuple (mempty, x)
  (MyTuple (a,f)) <*> (MyTuple (a',x)) = MyTuple (a <> a', f x)

newtype ListPrime a = ListPrime [a]
  deriving (Show, Eq)

newtype MyIO a = MyIO (IO a)

newtype MyTuple a b = MyTuple (a,b)
  deriving (Eq, Show)

newtype MyFunc x y = MyFunc (x -> y)
  deriving (Show)

newtype Identity a = Identity a
 deriving (Eq, Show)

data Pair a  = Pair a a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Three' a b  = Three' a b b deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

data Four' a b  = Four' a a a b deriving (Eq, Show)

instance Eq a => EqProp (ListPrime a) where
  (=-=) = eq

instance (Eq a, Eq b ) => EqProp (Four' a b) where
  (=-=) = eq

instance (Eq a, Eq b ,Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Eq a, Eq b ,Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Eq a, Eq b ) => EqProp (Three' a b ) where
  (=-=) = eq

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c x) = Four a b c (f x)

instance Functor (Three' a ) where
  fmap f (Three' a x x') = Three' a (f x) (f x')

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance Functor (MyFunc x) where
  fmap f (MyFunc f') = MyFunc $ f . f'

instance Functor ListPrime where
  fmap f (ListPrime l) = ListPrime (fmap f l)

instance Functor (MyTuple a) where
  fmap f (MyTuple (a,x)) = MyTuple  (a, f x)

instance Functor MyIO where
  fmap f (MyIO ioa) = MyIO $ fmap f ioa

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Monoid a => Functor (Four' a) where
  fmap f (Four' a b c x)  = Four' a b c $ f x

instance (Monoid a) => Functor (Two a) where
  fmap = liftA

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a b c f) <*> (Four' a' b' c' x) = Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Monoid a, Monoid b, Monoid c ) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' x) =  Four (a <> a') (b <> b') (c <> c') (f x)

instance (Monoid a) => Applicative (Three' a ) where
  pure x = Three' mempty x x
  (Three' a f f') <*> (Three' a' x  x') = Three' (a <> a') (f x) (f' x')

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b f) <*> (Three a' b'  x) =  Three (a <> a') (b <> b') (f x)

instance Monoid a => Applicative (Two a) where
 pure  = Two mempty
 (Two m f) <*> (Two m' x) = Two (m<>m') (f x)

instance Applicative Identity where
  pure  = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Applicative (MyFunc x) where
  pure = MyFunc . const
  (MyFunc f) <*> (MyFunc x) = MyFunc $ f <*> x

instance Applicative (ListPrime) where
  pure x   = ListPrime [x]
  (ListPrime fs) <*> (ListPrime xs) = ListPrime $ fs <*> xs

instance Applicative MyIO where
  pure x = MyIO $ return x
  (MyIO f) <*> (MyIO g) = MyIO $ f <*> g

instance Applicative Pair where
  pure x = Pair x x
  (Pair f f') <*> (Pair x x') = Pair (f x) (f' x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b ) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b ) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b , Arbitrary c ) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (ListPrime a) where
  arbitrary = ListPrime  <$> listOf arbitrary

instance Arbitrary a => Arbitrary (MyIO  a) where
  arbitrary = (MyIO . return)  <$>  arbitrary

instance Arbitrary a => Arbitrary  (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary  (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

m :: Four String String String (String, String,String)
m = undefined

o :: Four' String (String, String,String)
o = undefined

k :: Three String String (String, String,String)
k = undefined

l :: Three' String (String, String,String)
l = undefined

n :: ListPrime (Char,Char,Char)
n = mempty

i :: MyIO (Char,Char,Char)
i = undefined

g :: Identity (String, String, String)
g = undefined

h :: Pair (String, String, String)
h = undefined

j :: Two String (String, String, String)
j = undefined

test :: IO()
test = quickBatch $ applicative o
