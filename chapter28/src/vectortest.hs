module Main where

import Criterion.Main
import qualified Data.Vector as V

slice :: Int -> Int -> [a] -> [a]
slice from len xs =
    take len (drop from xs)

l :: [Int]
l = [1..10000]

v :: V.Vector Int
v = V.fromList l

testV' :: Int -> V.Vector Int
testV' n = V.map (+n) $ V.map (+n) $ V.map (+n) $ V.map (+n) (V.fromList [1..10000])

testV :: Int -> V.Vector Int
testV n = V.map ((+n) . (+n) . (+n) . (+n)) (V.fromList [1..10000])

main :: IO ()
main = defaultMain [
        bench "slicing list " $ whnf ( head . slice 100 900) l,
        bench "slicing vector " $ whnf (V.head . V.slice 100 900) v,
-- this will only work if you compile with optimisation wont work in a repl
-- use ```stack ghc -- -rtsopts -O2 vectortest.hs```
        bench "vector map prefused"  $ whnf testV 999,
        bench "vector map will be fused" $ whnf testV' 999]
