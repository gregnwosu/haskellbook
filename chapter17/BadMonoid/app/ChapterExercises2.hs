module ChapterExercises2 where

import Control.Applicative (liftA3)

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)



test = combos stops vowels stops
