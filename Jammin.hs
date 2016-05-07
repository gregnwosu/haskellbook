module Jammin where

import Data.List

data Fruit = Peach | Plum | Apple | Blackberry
             deriving (Eq, Show, Ord)

data JamJars = Jam {kind:: Fruit, quantity:: Int}
             deriving (Eq, Show, Ord)

row1 = [Jam Plum 3, Jam Apple 4, Jam Peach 1, Jam Blackberry 7]
row2 = [Jam Plum 8, Jam Apple 2, Jam Peach 1, Jam Blackberry 8]
row3 = [Jam Peach 15]
row4 = [Jam Plum 1, Jam Apple 0, Jam Peach 5, Jam Blackberry 19]
row5 = [Jam Plum 1, Jam Apple 0, Jam Peach 5, Jam Apple 19]
row6 = [Jam Plum 15, Jam Apple 2, Jam Peach 9, Jam Blackberry 2]
allJam = [row1, row2, row3, row4, row5, row6]
rowSum  = sum.(map quantity)
numjars = sum $ map rowSum allJam

mostRow :: JamJars
mostRow = foldr1 go (concat allJam)
 where
   go x m
      | quantity x > quantity m  = x
      | otherwise = m

sortJams :: [JamJars]
sortJams = sortBy (cmp kind)  (concat allJam)

cmp f x y  = compare (f x) (f y)
cmpEq f x y = cmp f x y == EQ

groupJam :: [[JamJars]]
groupJam = groupBy ( cmpEq kind) sortJams


