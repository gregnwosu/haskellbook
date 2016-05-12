module Cipher where


import Data.Char

shift ::  Int -> Char -> Char
shift n =  chr . (+97) . (`mod` 26) .  (+offset)  . ord
  where offset = 26 - (97 `mod` 26 ) + n

caesar :: Int -> String -> String
caesar n = map $ shift n . toLower

uncaesar :: Int -> String -> String
uncaesar = caesar . negate


userCaesar ::  IO String
userCaesar = do
  sh <- getLine
  string <- getLine
  return $ caesar (read sh) string
