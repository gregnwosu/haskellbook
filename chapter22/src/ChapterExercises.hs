module ReaderPractise where

import Control.Applicative
import Data.Maybe
import Data.Monoid

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

lookup' :: Eq a => a -> [(a , b )] -> Maybe b
lookup' a l =  let f True = First . pure
                   f _    = mempty
               in
                   getFirst $ foldMap (\(x,y) -> f (x==a) y) l

xs :: Maybe Integer
xs = lookup' 3 $ zip x y

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' k = lookup k $ zip x z

x1 = liftA2 (,) xs ys
x2 = liftA2 (,) ys zs
x3 =  (,) <$> z' <*> z'

summed :: Num c => (c,c ) -> c
summed = uncurry (+)



s' = summed <$> ((,) <$> xs <*> ys)

bolt :: Integer  -> Bool
bolt = liftA2 (&&) (>3) (<8)

main :: IO()
main = do
  print $ sequenceA [Just 3, Just 2 , Just 1]
  print $ sequenceA [x,y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> ys)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ and $ sequA 7
  print $ fromMaybe mempty $ sequA <$> s'
  print $ fromMaybe False $ bolt <$> ys
  let boltz' = (fmap . fmap ) bolt z'
  return ()

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m
