module Main where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
  putStrLn $ name  ++ " " ++ show acctNum

data WherePenguinsLive =
  Galapagos |
  Antartic  |
  Austrailia|
  SouthAfrica|
  SouthAmerica
  deriving (Eq, Show)

main::IO()
main = putStrLn "this is somehting"
