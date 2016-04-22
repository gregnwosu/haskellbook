{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

data Size  = Size Double deriving (Eq, Show)
data Price = Price Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size
               deriving (Eq, Show)
data Manufacturer =  Mini | Mazda | Tata
                     deriving (Eq , Show )

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
               deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane:: Vehicle -> Bool
isPlane = not . isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu


data Example = MakeExample Int deriving Show



newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)
newtype Tups = Tups (Int, String) deriving (Eq, Show, TooMany )

tooManyGoats ::  Goats -> Bool
tooManyGoats (Goats n) = n >42


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int,String)  where
  tooMany = tooMany . fst

instance TooMany (Int,Int) where
  tooMany (a,b) = tooMany (a + b)

instance (Num a, TooMany a) => TooMany (a,a) where
   tooMany (k, r) = tooMany k || tooMany r
