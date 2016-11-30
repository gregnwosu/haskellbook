{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ChapterExercises where
import Text.Printf
import Data.Char
import Text.Parser.Combinators
import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta
import Data.Time.Format
import Data.Time
import Data.Time.LocalTime
import Data.List
import Data.Word
import Text.Parser.Char

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
newtype CommentEntry = CommentEntry (Maybe String)
    deriving (Eq)
newtype Activity = Activity String
    deriving (Eq)

instance Show CommentEntry where
    show (CommentEntry (Just c)) = "-- " ++ c
    show _ = ""

instance Show Activity where
    show (Activity c) = c

data ActivityLine =
    ActivityLine TimeOfDay Activity CommentEntry
                 deriving (Eq)

instance Show ActivityLine where
    show (ActivityLine (TimeOfDay h m _) a c) =
        printf "%02d:%02d %s%s" h m  (show a) (go c)
               where
                 go :: CommentEntry -> String
                 go (CommentEntry Nothing) = ""
                 go c = printf " %s" (show c)


data LogEntry =
    DayLogEntry Day  CommentEntry [ActivityLine] | CommentLogEntry CommentEntry
        deriving (Eq)

instance Show LogEntry where
    show (DayLogEntry day c lines) =
        printf "\n# %04d-%02d-%02d%s%s" y m d (go c) l
              where (y,m,d) = toGregorian day
                    l = '\n':(intercalate "\n" $ show <$> lines)
                    go :: CommentEntry -> String
                    go ce@(CommentEntry (Just c)) =
                        printf " %s" (show ce)
                    go _ = ""
    show (CommentLogEntry c) = "\n" ++ show c

data LogFile =
     LogFile [LogEntry]
             deriving (Eq)
instance Show LogFile where
    show (LogFile entries) = tail $ concat $ show <$> entries

parseCommentEntry :: Parser CommentEntry
parseCommentEntry =  string "-- " *> (CommentEntry . Just <$>some (notChar '\n'))

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = TimeOfDay <$> (fromIntegral <$> integer) <* char ':' <*> (fromIntegral <$> integer) <*> pure 0

parseActivityLine :: Parser ActivityLine
parseActivityLine =  ActivityLine
                     <$> parseTimeOfDay
                     <*> (Activity <$> some ( notChar '\n'))
                     <*> (CommentEntry <$> (pure Nothing))

parseActivityLine' :: Parser ActivityLine
parseActivityLine' = ActivityLine
                    <$> parseTimeOfDay
                    <*> (Activity <$> (manyTill (notChar '\n') (string " -- ")))
                    <*> (CommentEntry . Just <$> some (notChar '\n'))

parseActivityLine'' :: Parser ActivityLine
parseActivityLine'' =  try parseActivityLine' <|> parseActivityLine

parseDay :: Parser Day
parseDay = fromGregorian <$>
           (toInt <$> count 4 digit) <* char '-' <*>
           (fromIntegral . toInt <$> count 2 digit) <* char '-' <*>
           (fromIntegral . toInt <$> count 2 digit)
parseLogEntry :: Parser LogEntry
parseLogEntry = (CommentLogEntry <$>
                 parseCommentEntry <* newline )
                <|>
                 DayLogEntry <$>
                 (string "# " *> parseDay ) <*>
                 (option (CommentEntry Nothing) (char ' ' *> parseCommentEntry ) <* newline) <*>
                 ( sepEndBy  parseActivityLine''  newline)
parseLogFile :: Parser LogFile
parseLogFile = LogFile <$> (some parseLogEntry  ) <* eof

exampleLog :: String
exampleLog = [r|-- wheee a comment
# 2025-02-05
08:01 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep|]

main :: IO()
main = do
  let Success(lf) = parseString parseLogFile mempty exampleLog
  putStrLn (show lf)
  let Success(lf') = parseString parseLogFile mempty (show lf)
  putStrLn $ "are they equal ? " ++ (show $ lf == lf')
-------------------- question 6
data IPAddress = IPAddress Word32
               deriving (Eq, Ord)

instance Show IPAddress where
    show = show
parseIPAddress :: Parser IPAddress
parseIPAddress = do
  quad1 <- integer
  char '.'
  quad2 <- integer
  char '.'
  quad3 <- integer
  char '.'
  quad4 <- integer
  return  $ IPAddress ( fromIntegral ((quad1 * (256^3)) +
                                      (quad2 * (256^2)) +
                                      (quad3 * (256^1)) +
                                      (quad4 * (256^0))))

testIPV4 = print $ parseString parseIPAddress mempty "172.16.254.1"
----------------- question 7
parseHexDigit :: Parser Int
parseHexDigit =
    ((subtract 48) . ord <$> oneOf ['0'..'9'])
    <|>
    ((subtract 87). ord <$> oneOf ['a'..'f'] )
    <|>
    ((subtract 55). ord <$> oneOf ['A'..'F'] )
parseHexDigits :: Parser Int
parseHexDigits = snd . foldr go (0,0)  <$> (some parseHexDigit)
                 where
                 go n (p,s) = (p+1, n * (16^p) + s)
toQuad :: [Int] -> Word64
toQuad l = fromIntegral ((l!!0 * ((16^4)^3)) +
                         (l!!1 * ((16^4)^2)) +
                         (l!!2 * ((16^4)^1)) +
                         (l!!3 * ((16^4)^0)))

parsePart6 :: Parser Int
parsePart6 = parseHexDigits  <* char ':'

-- all combinations of 0 - n compression of pure 0
parseColon :: Int -> Parser [Int]
parseColon n = foldr go seed [1..(n-1)]
               where seed = try $ count n parsePart6
                     go a b = b <|> try ((++) <$> ( count (n - a) parsePart6) <* char ':' <*> pure (replicate a 0) )

data IPAddress6 = IPAddress6 Word64 Word64

instance Show IPAddress6 where
    show (IPAddress6 h l) =  show $ ((fromIntegral h * (2^ 64)  + fromIntegral l ) :: Integer)

parserIPAddress6FromArray ::  Parser [Int] -> Parser IPAddress6
parserIPAddress6FromArray  = fmap (IPAddress6 <$> toQuad . (take 4 ) <*> toQuad . (drop 4 ) )

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = parserIPAddress6FromArray p
    where end = (count 1 parseHexDigits)
          seed = try ((++) <$> (count 7 parsePart6) <*> end <* eof)
          p = foldl go seed [0..7]
          go b n = b <|> try ((++) <$> (parseColon (7 - n))  <*> ((++) <$> (count n parsePart6) <*> end) <* eof)

testParseIPAddress6 = parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:ac10:fe01"
testParseIPAddress6'' = parseString parseIPAddress6 mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
testParseIPAddress6' = parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:cc78:f"
testParseIPAddress6''' = parseString parseIPAddress6 mempty "2001:DB8::8:800:200C:417A"

ipv4Toipv6 :: IPAddress -> IPAddress6
ipv4Toipv6 (IPAddress lw)= IPAddress6 0 (fromIntegral lw)


-------------------- Question 10

type Node = String
type GraphName = String
data Edge = LeafEdge Node () | BranchEdge Node Edge
          deriving (Show )
data GraphType = Graph | Digraph
               deriving (Show)
data DotGraph = DotGraph GraphType GraphName  [Edge]
              deriving Show

parseGraphType :: Parser GraphType
parseGraphType = (p "graph" Graph) <|> (p "digraph" Digraph)
                 where
                   p s g = try (spaces *> string s *> spaces *> pure g)
parseLeafEdge :: Parser Edge
parseLeafEdge = try $ LeafEdge <$> (spaces *>  some alphaNum) <*>  pure ()

parseBranchEdge :: Parser Edge
parseBranchEdge = try $ BranchEdge <$> (spaces *>  some alphaNum) <*> (spaces  *> string "->" *>  parseEdge)

parseEdge :: Parser Edge
parseEdge = (try parseBranchEdge <|> parseLeafEdge ) <* lineEnd
                where lineEnd = spaces *> option '?' (char ';') *> spaces *> (option '?' newline)

parseGraphName :: Parser String
parseGraphName = spaces *> (some alphaNum) <* spaces

parseDotGraph :: Parser DotGraph
parseDotGraph = DotGraph <$> parseGraphType <*> parseGraphName <*> parseEdges
                where parseEdges =
                                  between (symbol "{") (symbol "}") (some parseEdge)

graphExample = [r|
digraph G {
 main -> parse -> execute;
 main -> init;
 main -> cleanup;
 execute -> makestring;
 execute -> printf;
 init -> makestring;
 main -> printf;
 execute -> compare;
 }|]

testGraphExample = parseString parseDotGraph mempty graphExample
