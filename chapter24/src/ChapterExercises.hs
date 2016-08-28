module ChapterExercises where

import Data.Char
import Text.Parser.Combinators
import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta
import Data.Time.Format
import Data.Time
import Data.Time.LocalTime


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

parseMinusToNeg = (pure (-1) <$>char '-') <|> pure 1

base10Integer' :: Parser Integer
base10Integer' =  (*) <$> parseMinusToNeg <*> base10Integer

-- *************************************** QUESTION 4

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int


data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
                deriving (Eq, Show)

numbers n = toInt <$> count n digit

parseNumberPlan :: Parser NumberingPlanArea
parseNumberPlan = fromIntegral <$>  (option ""  ( string "1-")   *> (p <|> (between bo bc p)))
                where p = fromIntegral <$> numbers 3
                      bo = symbol "("
                      bc = symbol ")"
parseExchange :: Parser Exchange
parseExchange = option ' ' (oneOf " -")  *> (fromIntegral <$> numbers 3)
parseLineNumber :: Parser LineNumber
parseLineNumber = option ' ' (char '-')  *> (fromIntegral <$> numbers 3)
parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber <$> parseNumberPlan <*> parseExchange <*> parseLineNumber
-------------------------------------------------- question 5
type CommentEntry = String
type Activity = String
data ActivityLine = ActivityLine TimeOfDay Activity (Maybe CommentEntry)
data LogEntry = DayLogEntry Day [ActivityLine] | CommentLogEntry CommentEntry
data LogFile = LogFile [LogEntry]

parseCommentEntry :: Parser CommentEntry
parseCommentEntry = string "-- " *> some (notChar '\n')
parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = TimeOfDay <$> (fromIntegral <$> integer) <* char ':' <*> (fromIntegral <$> integer) <*> pure 0

parseActivityLine :: Parser ActivityLine
parseActivityLine = ActivityLine <$> parseTimeOfDay <* char ' ' <*> some anyChar <* notFollowedBy (string "--") <*> optional parseCommentEntry
 
parseDay :: Parser Day
parseDay = fromGregorian <$> (toInt <$> count 4 digit) <* char '-' <*> (fromIntegral . toInt <$> count 2 digit) <* char '-' <*> (fromIntegral . toInt <$> count 2 digit)
                    
parseLogEntry :: Parser LogEntry
parseLogEntry = (CommentLogEntry <$> parseCommentEntry) <|>
                (DayLogEntry <$> (string "# " *> parseDay)  <*> some parseActivityLine) 
