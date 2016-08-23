{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString as LBS
import Control.Applicative
import Text.RawString.QQ
import Data.Text
import Data.Scientific (floatingOrInteger)    
sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host":"wikipedia.org"},
  "whatisit": {"red":"intoothandclaw"}
}
|]
newtype Host = Host String
    deriving (Eq, Show)
type Annotation = String
data Color = Red Annotation | Blue Annotation | Yellow Annotation
           deriving (Eq, Show)
data TestData = TestData {
      section :: Host,
      what :: Color
    } deriving (Eq, Show)
instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _ =
        fail "Expected an object for Host"
instance FromJSON Host where
    parseJSON (Object v) = Host <$> v .: "host"
    parseJSON _ = fail "Expected an object for Host"
instance FromJSON Color where
    parseJSON (Object v) =
        (Red <$> v .: "red") <|>
        (Blue <$> v .: "blue") <|>
        (Yellow <$> v .: "yellow")

data NumberOrString =
    Numba Integer |
    Stringy Text
    deriving (Eq, Show)
instance FromJSON NumberOrString where
    parseJSON (Number i) =
        case floatingOrInteger i of
          (Left _) -> fail "Must be Integral"
          (Right i) -> pure $ Numba i
    parseJSON (String s) = pure $ Stringy s
    parseJSON _ = fail "NumberString must be a number or string"
main = do
  let d = decodeStrict sectionJson :: Maybe TestData
  print d