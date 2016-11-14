module ChapterExercises where

import Data.Char
makeNum = ord . toUpper
spaceShiftStream :: (Int -> Int -> Int) -> String -> String -> String
spaceShiftStream f ks (' ':ts) = ' ':spaceShiftStream f ks ts
spaceShiftStream f ks [] = []
spaceShiftStream f (k:ks) (t:ts) = let baseChr = subtract 65 . makeNum . toUpper
                                       textnum = baseChr t
                                       keynum = baseChr k
                                       shiftAmount = textnum `f` keynum  `mod` 26
                                   in chr ( 65 + shiftAmount ) : spaceShiftStream f ks ts


vigenère :: String -> String -> String
vigenère key   = spaceShiftStream (+) ( concat $ repeat key)

unvigenère :: String -> String -> String
unvigenère key   = spaceShiftStream (-) ( concat $ repeat key)
