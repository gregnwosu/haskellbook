module Main where

import Criterion.Main
import qualified Data.Map as M

genList :: Int -> [(String, String)]
genList n = go n []
  where
    go 0  xs = ("0", "0") : xs
    go n' xs = go (n' - 1) ((show n' , show n') : xs)

pairList :: [(String, String)]
pairList = genList 3

testMap :: M.Map String String
testMap = M.fromList pairList
-- this is weird the map lookup is slower than the assoc list
main :: IO ()
main = let
         p = testMap `seq` pairList
         m = testMap
       in defaultMain
       [ bench "lookup one thing , map"
         $ whnf (M.lookup "doesntexist") m,
         bench "lookup one thing , list"
         $ whnf (lookup "doesntexist") p]
