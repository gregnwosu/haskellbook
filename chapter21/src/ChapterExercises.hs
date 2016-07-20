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
  traverse f (Cons x xs) = let a = Cons <$> f x
                               b = traverse f xs
                           in a <*> b
