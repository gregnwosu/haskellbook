{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main :: IO ()
main = scotty 3000 $ do
         get "/:word" $ do
           beam  <- param "word"
           (ActionT
            . (ExceptT . liftM Right)
            . (\m -> ReaderT (const m))
            . \m -> StateT $  \s ->  return . (flip (,) s) =<<  m
            )
            (putStrLn "hello")
           html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


-- instance MonadTrans (ReaderT r) where
--    lift = ReaderT . const
-- const :: b -> a -> b
-- ReaderT ::  r -> m a -> Reader r m a
-- ReaderT . const :: a -> Reader r m a