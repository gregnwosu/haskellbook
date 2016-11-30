{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module RWCDemo where
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Monoid
import Data.Typeable
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ
import  Data.Aeson hiding (Null)
import qualified Data.ByteString.Char8 as LBSC
import UserTools

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDirectory TEXT,
   realName TEXT,
   phone TEXT)|]


allUsers :: Query
allUsers =
    "SELECT * from users"

getUserQuery :: Query
getUserQuery =
    "SELECT * from users where username = ?"


data DuplicateData =
     DuplicateData
     deriving (Eq,Show,Typeable)

instance Exception DuplicateData

type UserRow =
    (Null, Text, Text, Text,Text,Text)

getUser :: SQLite.Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- SQLite.query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

meRow :: UserRow
meRow = (Null,
         "gnwosu",       "/bin/zsh",
         "/home/gnwosu", "Greg Nwosu",
         "555-123-4567")

createDatabase :: IO ()
createDatabase = do
  conn <- SQLite.open "resources/finger.db"
  SQLite.execute_ conn createUsers
  SQLite.execute conn insertUser meRow
  rows <- SQLite.query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn

returnUsers :: SQLite.Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- SQLite.query_ dbConn allUsers
  let usernames =
          map username rows
      newlineSeparated =
          T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
    BS.concat [ "Login: ", e username, "\t\t\t\t",
                "Name: ", e realName,  "\n",
                "Directory: ", e homeDir, "\t\t\t",
                "Shell: ", e shell, "\n"]
    where e = encodeUtf8

returnUser :: SQLite.Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  maybe (putStrLn (" Couldnt find matching user for username" ++ show username)) (sendAll soc . formatUser) maybeUser

handleQuery :: SQLite.Connection -> Socket -> IO ()
handleQuery dbConn soc = recv soc 1024 >>= go
    where
          go "\r\n" = do
              returnUsers dbConn soc
          go name   =
              returnUser dbConn soc (decodeUtf8 name)

handleQueries :: SQLite.Connection -> Socket -> IO ()
handleQueries dbConn sock =
    forever $ do
      (soc, _) <- accept sock
      putStrLn "Got connection, handle query"
      handleQuery dbConn soc
      close soc

handleDBQueries :: SQLite.Connection -> Socket -> IO()
handleDBQueries conn sock = forever $ do
                     (soc, _) <- accept sock
                     putStrLn "Got connection, handle query"
                     json <- recv soc 1024
                     let maybeUser = decodeStrict   json
                     traverse (processUser  conn) maybeUser
                     close soc


getSock port = do
    addrinfos <- getAddrInfo
                (Just (defaultHints { addrFlags = [AI_PASSIVE]}))
                Nothing
                (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
    bind sock (addrAddress serveraddr)
-- only one connection open at a time
    listen sock 1
    return sock

main' :: IO ()
main' = withSocketsDo $ do
         sock1 <- getSock "79"
         sock2 <- getSock "78"
         conn <- SQLite.open "resources/finger.db"
         forkIO $ handleQueries conn sock1
         forkIO $ handleDBQueries conn sock2
         SQLite.close conn
         close sock1
         close sock2
