module ReplaceExperiment where

import Data.Char
replaceWithP :: (Show b) => b -> Char
replaceWithP b = case odd.ord.head.show$ b of
  True -> 'w'
  _ -> 'p'

lms :: [Maybe [Char]]
lms = [Just "Im going to alphabet st", Nothing, Just "yeah yeah come on momma"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP


liftedReplace :: (Show a, Functor f) => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace


twiceLifted :: (Functor f, Functor g, Show a) => f (g a) -> f (g Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f , Functor g , Functor h, Show a) => f (g (h a)) -> f (g (h Char))
thriceLifted = (fmap.fmap.fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
main ::IO()
main = do
  putStr "replacewithP' lms:"
  print (replaceWithP' lms)
  putStr "liftedReplace lms:"
  print (liftedReplace lms)
  putStr "liftedReplace' lms:"
  print (liftedReplace' lms)
  putStr "twicelifted lms:"
  print (twiceLifted lms)
  putStr "twicelifted' lms:"
  print (twiceLifted' lms)
  putStr "thricelifted lms:"
  print (thriceLifted lms)
  putStr "thricelifted' lms:"
  print (thriceLifted' lms)
