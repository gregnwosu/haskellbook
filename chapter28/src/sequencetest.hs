module SequenceTest where

import Criterion.Main
import qualified Data.Sequence as SQ

s :: SQ.Seq Int
s = SQ.fromList [1..100]

l :: [Int]
l = [1..100]


main = defaultMain [
        bench "list concat" $ whnf (l ++ ) [0],
        bench "seq concat " $ whnf (s SQ.>< ) (SQ.singleton 0)]
