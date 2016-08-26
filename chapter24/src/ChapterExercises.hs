module ChapterExercises where

import Data.Char
import Text.Parser.Combinators
import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer
                    deriving (Show, Eq)
instance Ord NumberOrString where
   (NOSS _) `compare` (NOSI _) = GT
   (NOSI _) `compare` (NOSS _) = LT
   x `compare`  y = compare x y

comparerels [] [] = EQ
comparerels (x:xs) (y:ys)
            | x /= y = compare x y
            | otherwise = comparerels xs ys

instance Ord SemVer where
    compare (SemVer mgr mnr pch rel meta) (SemVer mgr' mnr' pch' rel' meta')
            | mgr /= mgr' = compare mgr mgr'
            | mnr /= mnr' = compare mnr mnr'
            | pch /= pch' = compare pch pch'
            | length rel /= length rel' = compare (length rel') (length rel)
            | otherwise = comparerels rel rel'

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
            SemVer Major Minor Patch Release Metadata
                   deriving (Show, Eq)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = NOSS <$> some ( notChar '.') <|> (NOSI <$> integer )

parseRelease :: Parser Release
parseRelease =  (:) <$> (char '-' *> parseNumberOrString) <*> many ( char '.' *> parseNumberOrString)

parseMetaData :: Parser Metadata
parseMetaData =
   (:) <$> (char '+' *> parseNumberOrString) <*> many (char '.' *> parseNumberOrString)

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> integer <* char '.' <*> integer <* char '.' <*> integer  <*> (parseRelease <|> pure mempty) <*> (parseMetaData <|> pure mempty)

-- *************************************** QUESTION 2

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

toInt :: String -> Integer
toInt = fromIntegral . snd . foldr go (0,0) 
          where go c (p,s) = (p+1, (ord c - 48) * (10^p) + s)
                

base10Integer :: Parser Integer
base10Integer = toInt <$> some parseDigit
