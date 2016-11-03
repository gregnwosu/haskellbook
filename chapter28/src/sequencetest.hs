module SequenceTest where

import Criterion.Main
import qualified Data.Sequence as SQ

s :: SQ.Seq Int
s = SQ.fromList [1..9100]
lists :: [[Int]]
lists = replicate 10 [1..1000000]
seqs :: [SQ.Seq Int]
seqs = replicate 10 (SQ.fromList [1..1000000])
l1 = [0]
s1 = SQ.singleton 0
l :: [Int]
l = [1..9100]

main =
       defaultMain
         [
          -- bench "list concat" $ nf (l ++ ) l1,
          -- bench "seq concat " $ nf (s SQ.>< ) s1,
          -- bench "list mconcat " $ nf mconcat lists,
          -- bench "seqs mconcat" $ nf mconcat seqs,
          bench "list index" $ nf (\xs -> xs !! 9001) l,
          bench "seq index" $ nf (`SQ.index` 9001) s
         ]
