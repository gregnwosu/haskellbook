{-# LANGUAGE PartialTypeSignatures #-}
module StringToByteString where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8

--Manual unicode encoding of Japanese text
-- GHC Haskell allows UTF8 in source files
s :: String
s = "\12371\12435\12395\12385\12399\12289\20803\27671\12391\12377\12363\65311"

utf8ThenPrint :: B.ByteString -> IO ()
utf8ThenPrint =
    putStrLn . T.unpack . TE.decodeUtf8

    -- this wont work Char8 cannot encode a utf8 encoded strings!
    -- only works with ascii data which is 7 bit
throwsException :: IO ()
throwsException =
    utf8ThenPrint (B8.pack s)


-- we can create a  bytestring by packing text, but this will most likely be corrupt since the text contains utf8 encoding and its beingpacked by char8 packer
bytesByWayOfText :: B.ByteString
bytesByWayOfText = TE.encodeUtf8 (T.pack s)

-- letting utf8-string  do it for us
libraryDoesTheWork :: B.ByteString
libraryDoesTheWork = UTF8.fromString s

thisWorks :: IO()
thisWorks = utf8ThenPrint bytesByWayOfText

alsoWorks :: IO ()
alsoWorks = utf8ThenPrint libraryDoesTheWork

main = putStrLn s
