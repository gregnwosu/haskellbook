module SequenceTest where

import Criterion.Main
import qualified Data.Sequence as SQ

s :: SQ.Seq Int
s = SQ.fromList [1..100]

l :: [Int]
l = [1..100]

main = let l1 = [0]
           s1 = SQ.singleton 0
       in defaultMain
         [bench "list concat" $ nf (l ++ ) l1,
          bench "seq concat " $ nf (s SQ.>< ) s1]
