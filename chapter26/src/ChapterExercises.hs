module ChapterExercises where

import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
rDec :: Num a => Reader a a
rDec = subtract 1 <$> ask

rShow :: Show a => ReaderT a Identity String
rShow = show <$> ask

rPrintAndinc :: (Num a , Show a ) => ReaderT a IO a
rPrintAndinc =
    do
      n <- ask
      lift $ putStrLn ("Hi: " ++ show n)
      return ( n + 1)

sPrintIncAccum :: (Num a , Show a ) => StateT a IO String
sPrintIncAccum  =
    do
      n <- get
      lift $ putStrLn ("Hi: " ++ show n)
      put (n + 1)
      return (show n)
