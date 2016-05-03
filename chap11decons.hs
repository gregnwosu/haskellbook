module Deconstruct where

import Data.Char

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False


data Car = Car { make :: String , model :: String , year :: Integer}
           deriving (Eq, Show)
data Automobile = Null | Automobile Car

-- Yes = False
-- No = False
-- Both = False

-- Yes = True
-- No = False
-- Both = False

-- Yes = False
-- No = True
-- Both = False

-- Yes = True
-- No = True
-- Both = False

-- Yes = False
-- No = False
-- Both = True

-- Yes = True
-- No = False
-- Both = True

-- Yes = False
-- No = True
-- Both = True

-- Yes = True
-- No = True
-- Both = True

-- 1. 8
   -- 2. 16
   -- 3.4^4
   -- 4.8x
   -- 5.16
   -- 6. 65536n

shift ::  Int -> Char -> Char
shift n =  chr . (+65) . (`mod` 26) .  (+offset)  . ord
  where offset = 26 - (65 `mod` 26 ) + n

applyShift :: [Int] -> String -> ([Int], String)
applyShift shifts  = foldl go (shifts, [])
     where
       go  (sft:sfts , list) char = (sfts, list ++ [shift sft char])

vigenere :: String -> String -> String
vigenere plaintxt key = unwords . snd $ foldl go (shifts, []) (words   plaintxt)
   where shifts=map ( (-65 +)  .  ord  . toUpper) . cycle $ key
         go  (shs, result) string =
           let (shiftsLeft, newString) = applyShift shs string
               in (shiftsLeft, result ++ [newString])


vigenere2 ::  IO String
vigenere2 = do
   plaintxt <- getLine
   key <- getLine
   return $ vigenere plaintxt key

vigenere3 :: IO String
vigenere3  = do
   plaintxt <- getLine
   key <- getLine
   let go (shs, result) string =
           let (shiftsLeft, newString) = applyShift shs string
               in (shiftsLeft, result ++ [newString])
   let shifts=map ( (-65 +)  .  ord  . toUpper) . cycle $ key
   return $ unwords . snd $ foldl go (shifts, []) (words   plaintxt)



capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph =  unwords . snd . foldl go (True, []) . words
  where
    go :: (Bool, [String]) -> String -> (Bool, [String])
    go (True,result) word= (last word == '.' , result ++ [capitalizeWord word])
    go (False,result) word= (last word == '.' , result ++ [word])
