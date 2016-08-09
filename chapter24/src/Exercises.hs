module Exercises where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Monad.Trans.State.Lazy
import Data.Monoid
import Lib

oneFinished :: Parser ()
oneFinished = char '1' >> eof

oneTwoFinished :: Parser ()
oneTwoFinished = char '1' >> char '2'>> eof

oneTwoThree :: Parser String
oneTwoThree  = undefined


char' :: Char -> Parser Char
char' c = Parser $ f <$> (const True)
            where f = undefined
                  s = undefined
