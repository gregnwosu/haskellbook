module Lib
    ( someFunc
    ) where

import XMonad
import XMonad.Actions.Volume
import Data.Map.Lazy (fromList)

someFunc =
  xmonad def  {
    keys =
    keys def `mappend`
      \c -> fromList [
        ((0, xK_F6), lowerVolume 4 >> return ()),
        ((0, xK_F7), raiseVolume 4 >> return ())
                    ]
                       }
