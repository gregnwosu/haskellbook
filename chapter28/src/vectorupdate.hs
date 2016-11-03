module Main where

import Criterion.Main
import Data.Vector ((//))
import qualified Data.Vector as V

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
         where go 0 v = v
               go n v = go (n-1) (v // [(n,0)])

updates f = ((,)0) <$> f

batchList :: Int -> V.Vector Int
batchList n = vec // (updates [0..n])

batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec (updates $ V.fromList [0..n])



main :: IO ()
main = defaultMain [
        bench "slow" $ whnf slow 9998,
        bench "batch list " $ whnf batchList 9998,
        bench "batchvector " $ whnf batchVector 9998
       ]
