module Intermission where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta
import Data.Ratio

fracParser :: Parser Rational
fracParser = do
  n <- decimal
  _ <- char '/'
  d <- decimal
  return (n % d)

data IntOrRat = IntOrRat (Either Integer Rational)

decimalOrFraction :: Parser IntOrRat
decimalOrFraction = (try (IntOrRat . Left <$> decimal) <|> (IntOrRat . Right <$> fracParser))
