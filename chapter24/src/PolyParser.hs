{-# LANGUAGE OverloadedStrings #-}

module PolyParser where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "2/1"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  demoninator <- decimal
  case demoninator of
    0 -> fail "Denominator cannot be 0"
    _ -> return (numerator % demoninator)


main :: IO()
main = do
  print $ parseOnly parseFraction badFraction
  print $ parseOnly parseFraction shouldWork
  print $ parseOnly parseFraction shouldAlsoWork
  print $ parseOnly parseFraction alsoBad
  print $ parseString parseFraction mempty  badFraction
  print $ parseString parseFraction mempty  shouldWork
  print $ parseString parseFraction mempty  shouldAlsoWork
  print $ parseString parseFraction mempty  alsoBad
