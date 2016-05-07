module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
    $ (==) <$> ((morseToChar =<<)  . charToMorse ) <*> Just

main :: IO ()
main = quickCheck prop_thereAndBackAgain
