module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was " ++ show e)
  writeFile "resources/bbb" "hi"

main = do
  writeFile "resources/aaa" "hi" `catch` handler
  putStrLn "wrote to file"
