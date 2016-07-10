module Lib
    ( someFunc
    ) where

import Prelude hiding  (sum, product, elem , minimum , maximum , null, length , toList )
import Data.Foldable (foldMap, foldr, find)
import Data.Monoid
import Control.Applicative
someFunc :: IO ()
someFunc = putStrLn "someFunc"

sum :: (Foldable t, Num a) => t a -> a
--sum  = getSum . foldMap Sum
sum = foldr (+) 0

product :: (Foldable t , Num a ) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t , Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (x ==))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr go Nothing
  where go x Nothing = Just x
        go x (Just y) = Just $ min x y

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum all = getFirst $ foldMap go all
  where go x  = case find (>x) all of
          Nothing -> First $ Just x
          _ -> First Nothing
null :: Foldable t => t a -> Bool
null   = not . getAny . foldMap (const $Any True)

length :: (Foldable t ) => t a -> Int
length = getSum . foldMap (const $Sum 1)

toList :: (Foldable t ) => t a -> [ a]
toList = foldMap (:[])

fold :: (Foldable t , Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m ) -> t a -> m
foldMap' f  = foldr (mappend . f) mempty

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant x)  = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a ) where
  foldMap f (Three' a b b') = f b'

data Four' a b = Four' a b b b

instance Foldable (Four' a ) where
  foldMap f (Four' a b b' b'') = f b''

filterF :: (Applicative f, Foldable t, Monoid (f a)) => ( a -> Bool) -> t a  -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

