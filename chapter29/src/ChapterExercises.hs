module Main where

import Data.Char
import System.Environment
import System.IO

baseChr :: Char -> Int
baseChr = subtract 65 . ord . toUpper

transform :: (Int -> Int -> Int) -> String -> String -> String
transform f ks (' ':ts) = ' ':transform f ks ts
transform f _ [] = []
transform f (k:ks) (t:ts) = let textnum = baseChr t
                                keynum = baseChr k
                                shiftAmount = textnum `f` keynum  `mod` 26
                            in chr ( 65 + shiftAmount ) : transform f ks ts

vigenère :: String -> String -> String
vigenère key   = transform (+) ( concat $ repeat key)

unvigenère :: String -> String -> String
unvigenère key   = transform (-) ( concat $ repeat key)

key = "ALLY"
forArg ("d": _) = unvigenère key
forArg ("e":_) = vigenère key

timeoutMillis :: [String] -> IO Int
timeoutMillis (_:t:_) = return $ read t
timeoutMillis  _ =
    hPutStr stderr "\ntimeout not supplied default to 5000 ms" >>
    return 5000
nextChar :: Char -> IO String
nextChar c = isEOF >>= go
             where go True = return []
                   go False = (c:) <$> readStdInTillEOF


readStdInTillEOF :: IO String
readStdInTillEOF = do
  c <- getChar
  nextChar c

main :: IO ()
main = do
         crypt <- forArg <$> getArgs
         timeout <-  getArgs >>= timeoutMillis
         isTimedOut <- hWaitForInput stdin timeout
         if (isTimedOut)  then  ( putStrLn " ">> readStdInTillEOF >>= print . crypt)  else (hPutStr stderr "\ntimed out waiting for input")
