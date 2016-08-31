module Spec where



import Test.QuickCheck
import Test.Hspec
import ChapterExercises
import Data.Char
import Text.Parser.Combinators
import Control.Applicative
import Text.RawString.QQ
import qualified Text.Trifecta as T
import Data.Time.Format
import Data.Time
import Data.Time.LocalTime
import Data.Fixed
import Test.QuickCheck.Gen

instance Arbitrary TimeOfDay where
    arbitrary = TimeOfDay <$>  choose (0,23) <*> choose (0,59) <*> (MkFixed <$> choose (0,100))

instance Arbitrary CommentEntry where
    arbitrary = CommentEntry . Just <$> (elements $  show <$> [1..20])
                
instance Arbitrary ActivityLine where
    arbitrary = ActivityLine <$> arbitrary <*> (Activity <$> elements ["eating", "sleeping", "raving"]) <*> arbitrary

instance Arbitrary Day where
    arbitrary = fromGregorian <$> choose (1900, 2033) <*> choose (1,12) <*> choose (1,31)

instance Arbitrary LogEntry where
    arbitrary = frequency [(1, DayLogEntry <$> arbitrary <*> arbitrary <*> listOf1 arbitrary) , (1, arbitrary )]

instance Arbitrary LogFile where
    arbitrary = LogFile <$> vectorOf 10 arbitrary

thereAndBackAgain :: LogFile -> Bool
thereAndBackAgain lf = go (T.parseString parseLogFile mempty (show lf))
                       where go (T.Success lf') = show lf == show lf'
                             go _ = False


test :: IO ()
test  =  quickCheck thereAndBackAgain
