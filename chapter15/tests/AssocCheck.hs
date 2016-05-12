module AssocCheck where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssoc a b c = (a <> b)  <> c == (a <> (b <> c))

monoidLeftIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidRightIdentity a = (a <> mempty ) == a
