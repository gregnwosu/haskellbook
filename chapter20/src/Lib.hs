module Lib
    ( someFunc
    ) where

import Prelude hiding  (sum, product, elem , minimum , maximum , null, length , toList )
import Data.Foldable (foldMap, foldr)
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
minimum = foldr (fmap . min) Nothing




--maximum :: (Foldable t, Ord a) => t a -> Maybe a
