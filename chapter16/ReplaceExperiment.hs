module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP


liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace


twiceLifted :: (Functor f, Functor g) => f (g a) -> f (g Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f , Functor g , Functor h) => f (g (h a)) -> f (g (h Char))
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
