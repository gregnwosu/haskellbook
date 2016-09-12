{-# LANGUAGE InstanceSigs #-}

module Exercises where

newtype Compose f g a =
    Compose { getCompose :: f (g a)}
    deriving ( Eq, Show)
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f , Applicative g) =>
    Applicative (Compose f g) where
        pure :: a -> Compose f g a
        pure a = Compose (pure (pure a))
        (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
        (Compose f ) <*> (Compose a ) = Compose $  (<*>) <$> f  <*> a


instance (Monad f , Monad g) => Monad (Compose f g) where
    return = pure
    (>>=) :: Compose f g a  -> (a -> Compose f g b) -> Compose f g b
    (>>=) (Compose fga) a2fgb = undefined

instance (Foldable f, Foldable g) =>
    Foldable (Compose f g) where
        foldMap f (Compose fga ) = (foldMap . foldMap) f fga
instance (Traversable f, Traversable g) =>  Traversable (Compose f g) where
    traverse f (Compose fga )  =Compose <$> (traverse  . traverse) f fga

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c ) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap fab fcd (Deux a c) = Deux (fab a) (fcd c)

data Const a b = Const a

instance Bifunctor Const where
    bimap fab fcd (Const a) = Const (fab a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap fab fcd (Drei x a c) = Drei x (fab a) (fcd c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap fab fcd (SuperDrei a a') = SuperDrei a (fab a')

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap fab fcd (SemiDrei a ) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap fab fcd (Quadzzz a' b' a c) = Quadzzz a' b' (fab a) (fcd c)

data Either' a b = Left' a | Right' b

instance Bifunctor Either'   where
    bimap fab fcd (Left' a) = Left' (fab a)
    bimap fab fcd (Right' c) = Right' (fcd c)                               
                