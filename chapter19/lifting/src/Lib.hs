module Lib
    ( someFunc
    ) where

import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

someFunc :: IO ()
someFunc = putStrLn "someFunc"

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) getCurrentTime

textUuid :: IO Text
textUuid = fmap (T.pack . UUID.toString) UUIDv4.nextRandom


