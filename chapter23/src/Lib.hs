module Lib
    ( someFunc
    ) where

import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    -> Die
-> Die
-> Die
-> Die
-> Die
-> Die
    

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1,6) s
      (d2, s2) = randomR (1,6) s1
      (d3, _) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
