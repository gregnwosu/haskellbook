{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import Web.Scotty
import Data.Monoid (mconcat)

-- works if you go to http://localhost:3000/word?word=3
someFunc :: IO ()
someFunc = scotty 3000 $
  get ":/word" $ do
    beam <- param "word"
    html (mconcat
     ["<h1>Scottty, ",
      beam,
      "me up! </h1>"
     ])
