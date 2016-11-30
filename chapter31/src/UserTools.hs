{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified Database.SQLite.Simple as SQLite
import Control.Monad.Trans.Maybe
import Network.Socket hiding (recv)
import RWCDemo hiding (main)
import Database.SQLite.Simple.Types
import Text.RawString.QQ
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)
import  Data.Aeson hiding (Null)
import qualified Data.ByteString as LBS
import qualified Data.ByteString.Char8 as LBSC
modifyUserSql ::  Query
modifyUserSql = [r|
                UPDATE users set
                username = ?,
                shell = ?,
                homeDirectory = ?,
                realName = ?,
                phone = ? where id = ?|]

modifyUser :: SQLite.Connection -> User -> IO ()
modifyUser conn user  = do
   SQLite.execute conn modifyUserSql
       (username user,
        shell user,
        homeDirectory user,
        realName user,
        phone user, userId user)
   return ()

addUser :: SQLite.Connection -> User -> IO ()
addUser conn  =  SQLite.execute conn insertUser

addUserFromJson :: SQLite.Connection -> String -> IO (Maybe ())
addUserFromJson conn userJson =
   let maybeUser = decodeStrict  (LBSC.pack userJson)
   in  traverse (addUser conn ) maybeUser

modifyUserFromJson :: SQLite.Connection -> String -> IO (Maybe ())
modifyUserFromJson conn userJson =
   let maybeUser = decodeStrict  (LBSC.pack userJson)
   in  traverse (modifyUser conn) maybeUser

modifyUserFromJson' json =  do
  conn <- SQLite.open "resources/finger.db"
  modifyUserFromJson conn json
  SQLite.close conn

addUserFromJson' json =  do
  conn <- SQLite.open "resources/finger.db"
  addUserFromJson conn json
  SQLite.close conn


handleQueries [] = print "no argument specified"
handleQueries _ =  print "too many arguments"

youRow = "{\"phone\":\"12324\",\"realName\":\"gregory nwosu\",\"username\":\"gregory\",\"shell\":\"/bin/bash\",\"userId\":1,\"homeDirectory\":\"/home/greg\"}"

main :: IO()
main = do
   let u = (User 1 "gregory" "/bin/bash" "/home/greg" "gregory nwosu" "12324" )
   print u
   print  (encode  u )
