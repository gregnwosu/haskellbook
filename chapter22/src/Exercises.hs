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
    dogsAddress :: Address
      } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu" ) (Address "Austin")
liftA2' :: Applicative f =>  (a -> b -> c) -> f a -> f b -> f c
liftA2' f =  (<*>) . (f <$>)


asks :: (r -> a ) -> Reader r a
asks = Reader
