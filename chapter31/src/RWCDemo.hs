{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module RWCDemo where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Monoid
import Data.Typeable
--import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ
import  Data.Aeson hiding (Null)

data User =
     User {
      userId :: Integer,
      username :: Text,
      shell :: Text,
      homeDirectory :: Text,
      realName :: Text,
      phone :: Text
     }
     deriving (Eq, Show)

instance SQLite.FromRow User where
    fromRow = User <$> SQLite.field
                   <*> SQLite.field
                   <*> SQLite.field
                   <*> SQLite.field
                   <*> SQLite.field
                   <*> SQLite.field
instance SQLite.ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        SQLite.toRow (id_, username, shell, homeDir, realName, phone)


instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "userId"
                                <*> v .: "username"
                                <*> v .: "shell"
                                <*> v .: "homeDirectory"
                                <*> v .: "realName"
                                <*> v .: "phone"

instance ToJSON User where
    toJSON (User userId' username' shell' homeDirectory' realName' phone') =
        object  [
     "userId" .= userId',
     "username" .= username',
     "shell" .= shell',
     "homeDirectory" .= homeDirectory',
     "realName" .= realName',
     "phone" .= phone' ]

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDirectory TEXT,
   realName TEXT,
   phone TEXT)|]

insertUser :: Query
insertUser =
    "INSERT INTO users VALUES (?,?,?,?,?,?)"

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
    where go _ = returnUsers dbConn soc
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
      sClose soc

main' :: IO ()
main' = withSocketsDo $ do
         addrinfos <- getAddrInfo
                     (Just (defaultHints { addrFlags = [AI_PASSIVE]}))
                     Nothing
                     (Just "79")
         let serveraddr = head addrinfos
         sock <- socket (addrFamily serveraddr)
                Stream defaultProtocol
         bind sock (addrAddress serveraddr)
         listen sock 1
         -- only one connection open at a time
         conn <- SQLite.open "resources/finger.db"
         handleQueries conn sock
         SQLite.close conn
         sClose sock
