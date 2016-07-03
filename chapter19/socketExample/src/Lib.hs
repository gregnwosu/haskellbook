
module Lib
    ( someFunc) where

import Network.Socket
import qualified Filesystem.Path.CurrentOS as F (encodeString, FilePath)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

openSocket :: F.FilePath -> IO Socket
openSocket p = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix . F.encodeString $ p
  return sock
