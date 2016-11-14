module WhatHappens2 where

import Control.Concurrent
import System.IO.Unsafe


main' :: IO ()
main' = do
  mv  <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero


myData :: MVar Int
myData = unsafePerformIO newEmptyMVar

main :: IO ()
main = do
  putMVar myData 0
  zero <- takeMVar myData
  print zero
