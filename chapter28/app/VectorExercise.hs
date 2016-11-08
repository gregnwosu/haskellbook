module Main where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

appendVectorUnboxed :: Int -> UV.Vector Int
appendVectorUnboxed  0 = UV.singleton 0
appendVectorUnboxed  n = UV.singleton n UV.++ appendVectorUnboxed (n - 1)

appendVector :: Int -> V.Vector Int
appendVector  0 = V.singleton 0
appendVector  n = V.singleton n V.++ appendVector (n - 1)


main :: IO ()
main = defaultMain [
        bench "appendVector" $ whnf appendVector 1000,
        bench "appendVectorUnboxed" $ whnf appendVectorUnboxed 1000
       ]
