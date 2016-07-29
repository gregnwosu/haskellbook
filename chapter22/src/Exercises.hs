{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.Functor

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader f') = Reader $ f . f'

ask :: Reader a a
ask = Reader id

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype  Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address} deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName,
    dogsAddress :: Address } deriving (Eq, Show)

getDogR' :: Person -> Dog
getDogR' = liftA2' Dog dogName address

getDogR :: Reader Person Dog
getDogR =  Reader $ Dog <$> dogName <*> address

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu" ) (Address "Austin")
liftA2' :: Applicative f =>  (a -> b -> c) -> f a -> f b -> f c
liftA2' f =  (<*>) . (f <$>)


asks :: (r -> a ) -> Reader r a
asks = Reader


instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra

foo :: (Functor f , Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r , length r)

barOne :: Foldable t => t a -> (t a , Int)
barOne r = (r , length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a],Int)
frooty' = \r -> bar (foo r) r

fooBind m k  =  \r -> k (m r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy


getDogRM' :: Reader Person Dog
getDogRM' = Dog <$> dogName <*> address <$> ask

  
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ runReader <$> (aRb . ra ) <*> id
