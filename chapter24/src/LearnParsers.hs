module LearnParsers where

import Text.Trifecta
import Control.Monad.Trans.State.Lazy
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

strs :: String -> Parser String
strs (x:xs)  = do char x >> strs xs

oneTwoThree :: Parser String
oneTwoThree = choice [string "123" >> eof >> return "123", string "12">> eof >> return "12", string "1" >> eof >> return "1"]

intEof :: String -> Result Integer
intEof = parseString (integer >>= \i -> eof >> return i) mempty


main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
