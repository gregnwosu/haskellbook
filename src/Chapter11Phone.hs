module Chapter11Phone where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
data DaPhone = DaPhone [String]

type Digit = Char
type Presses = Int
type CharCode = (Digit, Presses)

phoneKeys:: DaPhone
phoneKeys =
  DaPhone [
    "1",     "2abc", "3def",
    "4ghi",  "5jkl", "6mno",
    "7pqrs", "8tuv", "9wxyz",
    "*^",    "0+ _", "#.," ]
shiftCase::CharCode
shiftCase = ('*', 1)

reverseTaps :: DaPhone -> Char  ->  [CharCode]
reverseTaps (DaPhone keys) char  = fromMaybe (error$ toLower char:"notfound")  . go (isUpper char) .  find (elem lchar)$ keys
  where
   lchar = toLower char
   go True  ms = (shiftCase:) <$> go False ms
   go False ms = do
     s <- ms
     let digit = head s
     presses <- elemIndex lchar s
     return [(digit,presses)]

cellPhonesDead :: DaPhone -> String -> [CharCode]
cellPhonesDead  dp  = concat . fmap  (reverseTaps dp)

fingerTaps :: [CharCode] -> Presses
fingerTaps = sum . fmap snd

mostPopularLetter :: String -> Char
mostPopularLetter =  head . maximumBy (compare `on` length)  .  group . sort


mostPopularLetterCost :: String -> Int
mostPopularLetterCost s = fingerTaps popCodesOnly
  where popCode = reverseTaps phoneKeys .  mostPopularLetter $ s
        codes   = fmap (reverseTaps phoneKeys) s
        popCodesOnly = concat $ filter (==popCode) codes

coolestLtr :: (Ord a) => [[a]] -> a
coolestLtr = head . maximumBy (compare `on` length). group . sort . concat

coolestWord :: [String] -> String
coolestWord =  coolestLtr . fmap words
