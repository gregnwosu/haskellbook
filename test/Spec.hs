module Main where

import Test.QuickCheck
import Cipher
import Data.Char

caesarProp = forAll (caesarGen) go
   where go (offset, string )=
           let ciphertext = caesar offset string
           in uncaesar offset ciphertext == fmap toLower string

caesarGen :: Gen (Int, String)
caesarGen = (,) <$> arbitrary  <*> listOf  ( elements ['a'..'z'])

main :: IO ()
main = quickCheck caesarProp
