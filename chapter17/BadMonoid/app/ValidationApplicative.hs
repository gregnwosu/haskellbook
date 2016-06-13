module ValidationApplication () where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
  First a |
  Second b
  deriving (Eq, Show)

data Validation e a =
  Failure e |
  Success a
  deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a ) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

data Errors =
  DividedByZero | StackOverflow | MooglesChewedWires
    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure err) = Failure err
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e ) where
  pure = Success
  (Failure x) <*> (Failure y)  = Failure ( x <> y)
  _ <*> (Failure y) = Failure y
  (Failure x) <*> _ = Failure x
  (Success f) <*> (Success x) = Success (f x)

instance (Arbitrary a, Arbitrary e)  => Arbitrary (Validation e a) where
  arbitrary =  frequency [(1, Failure <$> arbitrary), (2, Success <$> arbitrary)]

instance  (Eq e , Eq a) => EqProp (Validation e a) where
   (=-=)  =  eq

s :: Validation [Int] (String,String,String)
s = Success ("","","")

test2 :: IO()
test2 = quickBatch (applicative s)
