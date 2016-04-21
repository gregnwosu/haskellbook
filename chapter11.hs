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
