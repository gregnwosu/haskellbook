module HttpStuff where

import Data.ByteString.Lazy
import Network.Wreq



urls :: [String]
urls = ["http://httpbin.org/ip", "http://httpbin.org/bytes/5"]

mapping ::  IO [Response ByteString]
mapping = traverse get urls
