module ChapterExercises where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a  = Identity a
 deriving (Eq, Ord, Show)

data Nope a = NopeDotJpg
  deriving (Eq, Show)

data PhEither b a =
  Left' a | Right' b
    deriving (Eq, Show)

data List a = Nil | Cons a (List a)
   deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Functor (PhEither b) where
  fmap _ (Right' a) = Right' a
  fmap f (Left' b) =  Left' $ f b

instance Functor Identity where
  fmap f (Identity x)  = Identity $ f x

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) (f <$> l)

instance Monoid (List a) where
  mempty  = Nil
  Nil `mappend` x = x
  x `mappend` Nil = x
  Cons a as `mappend` ys =   Cons a $ as `mappend` ys

instance Applicative List where
  pure x  = Cons x Nil
  Nil <*> _  = Nil
  _ <*> Nil = Nil
  Cons f fs <*> (Cons x xs) =
    Cons (f x) $ (pure f <*> xs) `mappend` (fs <*> pure x)

instance Applicative Identity where
  pure = Identity
  Identity f  <*> Identity x = Identity $ f x

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Applicative (PhEither b) where
  pure  = Left'
  (Right' x) <*> _ = Right' x
  _ <*> (Right' x)  = Right' x
  (Left' f) <*> (Left' x) = Left' $ f x

instance Monad Nope where
  return  = pure
  _ >>= _ = NopeDotJpg

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance Monad (PhEither b) where
  return  = pure
  (Right' x) >>= _ = Right' x
  (Left' x) >>= f = f x

instance Monad List where
  return  = pure
  Nil >>= _ = Nil
  m >>= f = join $ fmap f m
   where
     join Nil = Nil
     join (Cons x xs) = x `mappend` join xs

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
 
instance EqProp (Nope a) where
  (=-=) = eq

instance (Eq a , Eq b) => EqProp (PhEither a b) where
  (=-=) = eq

instance (Eq a) => EqProp (List a ) where
  (=-=) = eq

nope :: Nope (String, String , String)
nope = undefined

identi :: Identity (String , String , String)
identi  = undefined

pheither  :: PhEither String  (String, String , String)
pheither  = undefined

list  :: List  (String, String , String)
list  = undefined

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Arbitrary a =>  Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a =>  Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (1, liftA2 Cons arbitrary arbitrary)]

instance (Arbitrary a, Arbitrary b ) => Arbitrary (PhEither b a)  where
  arbitrary = frequency [ (1, Right' <$> arbitrary), (1, Left' <$> arbitrary)]

main :: IO()
main = do
  quickBatch $ functor pheither
  quickBatch $ applicative pheither
  quickBatch $ monad pheither
  quickBatch $ applicative nope
  quickBatch $ functor  nope
  quickBatch $ monad nope
  quickBatch $ applicative identi
  quickBatch $ functor identi
  quickBatch $ monad identi
  quickBatch $ functor list
  quickBatch $ monoid  list
  quickBatch $ applicative list
  quickBatch $ monad list
