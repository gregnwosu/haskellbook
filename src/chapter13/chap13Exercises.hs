module Chap13Exercises where

import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome =
  let go a
         | toLower a `elem` ['a'..'z'] = (toLower a :)
         | otherwise = id
  in forever $ do
  line1 <- fmap (foldr go []) getLine
  case (line1 == reverse line1) of
    True -> putStrLn "its a palindrome!"
    False -> putStrLn "Nope"


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        "Age was: " ++ show age


gimmePerson :: IO()
gimmePerson = do
  putStr "enter name: "
  name <- getLine
  putStr "enter age: "
  age <- fmap (read) getLine
  let maybePerson = mkPerson name age
  case maybePerson of
    Right person -> putStrLn $ "Yay! Successfully got a person" ++ show person
    Left errMsg -> putStrLn $ "whoops ," ++ show errMsg
