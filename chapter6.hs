module Chapter6 where

import Data.List

data Trivial = Trivial

instance Eq Trivial where
  Trivial == Trivial = True


data DayOfWeek = Mon  | Tue | Wed | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) _ _ = False
data Boo = Boo
data Identity a =
  Identity a



instance (Eq a) => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'


data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
   TisAn a == TisAn b = a == b


data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  TisAnInt a == TisAnInt b = a == b
  TisAString a == TisAString b = a ==b
  _ == _ = False


data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  ThisOne a == ThisOne b = a == b
  ThatOne a == ThatOne b = a == b
  _ == _ = False

data EitherOr a b = Hello a | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello b = a == b
  GoodBye a == GoodBye b = a ==b
  _ == _ = False


addWeird :: (Ord a, Num a) => a -> a-> a
addWeird x y =
  if x > 1
    then
      x + y
    else
      x
freud :: Int -> Int
freud x = x
myX :: Int
myX = 1


jung :: [Int] -> Int
jung xs = head (sort xs)


young :: Ord a => [a] -> a
young xs = head (sort xs)


mySort :: [Char] -> [Char]
mySort = sort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b


arith :: Num b => (a -> b) -> Integer -> a -> b
arith f b a =  f a + (fromIntegral b)
