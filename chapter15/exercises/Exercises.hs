module Exercises where

import Data.Monoid
import Test.QuickCheck
-- an algebra is a set of laws around a set of operations, and how the operate in relation to each other and a specific type


data Booly a  = False' | True' deriving (Eq, Show)

monoidAssoc :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssoc a b c = (a <> b)  <> c == (a <> (b <> c))

monoidLeftIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidRightIdentity a = (a <> mempty ) == a


instance Monoid (Booly a ) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'
  mempty = False'


data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (mappend a b)
  mappend Nada Nada = Nada
  mappend Nada x = x
  mappend x _ = x

newtype First' a = First' { getFirst :: Optional a}
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    opVal <- arbitrary
    frequency [(1, return $ First' Nada), (1, return ( First' $ Only opVal))]

instance Monoid (First' a) where
  mempty = First' Nada
  mappend x (First' Nada) = x
  mappend (First'  Nada) x = x
  mappend a _ = a

main  :: IO ()
main = do
  quickCheck (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  quickCheck (monoidLeftIdentity :: First' String ->  Bool)
  quickCheck (monoidRightIdentity :: First' String ->  Bool)
