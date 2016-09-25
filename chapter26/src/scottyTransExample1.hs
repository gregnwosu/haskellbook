{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe ( fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty

paramold' :: Parsable a => Text -> ActionM (Maybe a)
paramold' k = rescue (Just <$> param k ) (const (return Nothing))

main = scotty 3000 $
         get "/:word " $ do
          beam' <- paramold' "word"
          let beam = fromMaybe "" beam'
          i <- paramold' "num"
          liftIO $ print ( i:: Maybe Integer)
          html $ mconcat ["<h1> Scotty, ", beam , " me up! </h1>"]


param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $
            rescue (Just <$> param k)
                   (const (return Nothing))

main' = scotty 3000 $ do
         get "/:word" $ do
          beam <- param' "word"
          reco <- runMaybeT $ do
            a <- param' "1"
            b <- param' "2"
            c <- param' "3"
            d <- param' "4"
            (lift . lift ) $ print b
            return (liftIO $ pure (a,b,c,d))
          liftIO $ print reco
          html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
