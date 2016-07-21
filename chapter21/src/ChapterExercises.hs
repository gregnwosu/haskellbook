{-# LANGUAGE FlexibleContexts #-}
module ChapterExercises where

import Data.Monoid ((<>))

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) =  Identity <$> f x

newtype Constant a b =
  Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldr _ z _  = z

instance Traversable (Constant a) where
  traverse f (Constant x ) = pure $ Constant x

data Optional a = Nada | Yep a
   deriving (Eq, Show , Ord)

instance Foldable Optional where
  foldr f z (Yep x) = f x z
  foldr f z Nada = z

instance Functor Optional  where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a =
  Nil | Cons a (List a) deriving (Eq, Show, Ord)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (f<$>l)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons a l) =  f a $ foldr f z l

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) ((pure f <*> xs) <> (fs <*> pure x))

instance Monoid (List a) where
  mempty = Nil
  x `mappend` Nil = x
  Nil `mappend` x = x
  Cons a as `mappend` l = Cons a( as `mappend` l)

instance Traversable List where
  traverse f Nil = pure mempty
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

data Three a b c = Three a b c
  deriving (Eq, Show , Ord)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldr f z (Three a b c ) = f c z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' x) = Three (a <> a') (b <> b') (f x)

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Three' a b = Three' a b b
  deriving (Eq, Ord, Show)

instance Functor (Three' a ) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldr f z (Three' a b b') = f b $ f b' z

instance Traversable (Three' a) where
  traverse f (Three' a b b') =  Three' a <$> f b <*> f b'

data S n a  = S (n a) a
  deriving (Show , Ord , Eq )

instance Functor n => Functor (S n) where
  fmap f (S fx x) =  S (f <$> fx) (f x)

instance Foldable n => Foldable (S n) where
  foldr f z (S fx x) = foldr f (f x z) fx

instance Traversable n => Traversable (S n) where
  traverse a2fb (S na a) = S <$> traverse a2fb na <*> a2fb a

data Tree a =
  Empty | Leaf a | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Monoid a => Monoid (Tree a) where
  mempty = Empty
  x `mappend` Empty = x
  Empty `mappend` x = x
  l `mappend` r = Node l mempty r

-- instance  Foldable Tree where
--   foldMap _ Empty = mempty
--   foldMap f (Leaf x) = f x
--   foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Foldable Tree where
  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node l x r) = foldr f (f x ( foldr f z r)) l


instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
