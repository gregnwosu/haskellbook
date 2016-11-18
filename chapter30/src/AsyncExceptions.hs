module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "resources/test.dat"  AppendMode
  threadDelay 1000
  hPutStr h (replicate 100000000 '0' ++ "abc")
  hClose h


data PleaseDie =
     PleaseDie deriving Show

instance Exception PleaseDie

main' :: IO ()
main' = do
  threadId <- forkIO openAndWrite
  threadDelay 1500
  throwTo threadId PleaseDie

-- use mask from Control.Exception to mask or delay exceptions thrown to the child thread until the io action was complete

-- not that since openAndWrite is the last thing the child does , then the exception that we attempt to throw to the child thread is thrown in the main thread instead, it essentially blows up in our face!

-- with async we dont try to catch everything in general more time shoudl be spend making sure we have a good process supervisosr and good logs
main :: IO ()
main = do
  threadId <- forkIO (mask_ openAndWrite)
  threadDelay 1500
  throwTo threadId PleaseDie
