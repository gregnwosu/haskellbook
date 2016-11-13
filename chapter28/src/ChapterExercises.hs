{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Criterion.Main
newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL $ const []

singleton :: a -> DList a
singleton x = DL (x:)
{-# INLINE singleton#-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x:)
{-# INLINE snoc #-}

infixl `append`
append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
              where go 0 xs = xs
                    go n xs = go (n - 1) ([n] ++ xs)

toList :: DList Int -> [Int]
toList xs = unDL xs []

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
                   where go 0 xs = xs
                         go n xs = go (n - 1) (singleton n `append` xs)

-- this only works for compilation
-- stack ghc -- -fprof-auto  -rtsopts -O2 ChapterExercises.hs
main :: IO ()
main = defaultMain
       [
        bench "concat list"  $ whnf schlemiel 123456,        bench "concat dlist" $ whnf constructDList 123456]
