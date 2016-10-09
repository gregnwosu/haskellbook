{-# LANGUAGE  OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
--import Control.Monad.Trans.Maybe
--import qualified Control.Monad.Trans.State.Lazy as ST
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Web.Scotty.Internal.Types

data Config = Config { counts :: IORef (M.Map  Text Integer), prefix :: Text}

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
            -> M.Map Text Integer
            ->  (M.Map Text Integer, Integer)
bumpBoomp k m = let v'  = maybe 1 (+1) (M.lookup k m)
                    m' =  M.insert k v' m
                in (m', v')
action' :: Handler ()
action' = do
        unprefixed   <-  param "key"
        config <- lift ask
        let prefix' = prefix config
        let key' = mappend prefix' unprefixed
        counts' <- liftIO $  readIORef $ counts config

        let (newMap, result) =   bumpBoomp key' counts'
        liftIO $ writeIORef (counts config) newMap
        html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show result , "</h1>"]

app :: Scotty ()
app =  get "/:key" action'

main :: IO ()
main = do
   -- [prefixArg] <- getArgs
   let prefixArg = "ook"
   counter <- newIORef M.empty
   let config = Config counter ""
       runR = (`runReaderT` config)
   scottyT 3000 runR app
