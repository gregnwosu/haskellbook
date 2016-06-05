module Exercises where

import Data.List (elemIndex)
import Control.Applicative
import Data.Monoid

added:: Maybe Integer
added = (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

y' :: Maybe Integer
y' = lookup 3 $ zip [1,2,3] [4,5,6]

z' :: Maybe Integer
z' = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z'

x'' :: Maybe Int
x'' = elemIndex 3 [1,2,3,4,5]

y'' :: Maybe Int
y'' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x'' <*> y''

xs''' = [1,2,3]
ys''' = [4,5,6]

x''' :: Maybe Integer
x''' = lookup 3 $ zip xs''' ys'''

y''' :: Maybe Integer
y''' = lookup 2 $ zip xs''' ys'''


summed = liftA sum $ (,)  <$> x''' <*> y'''


newtype Identity a = Identity a
   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f  (Identity x)= Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq , Show, Ord)

instance Functor (Constant a ) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant $ a `mappend` b

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String ->  Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n  of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' ->
          Just $ Person n' a'

data Cow = Cow { name :: String , age :: Int , weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
 | n >= 0 = Just n
 | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' = Cow <$> noEmpty name'
                                       <*> noNegative age'
                                       <*> noNegative weight'
ex2_1 ::  Maybe String
ex2_1 = const <$> Just "Hello" <*> pure "World"

ex2_2 :: Maybe (Integer, Integer, String , [Integer])
ex2_2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
