{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module UserTools where
import qualified Database.SQLite.Simple as SQLite
import Data.Text (Text)
import Control.Monad.Trans.Maybe
import Network.Socket hiding (recv)
import Database.SQLite.Simple.Types
import Text.RawString.QQ
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)
import  Data.Aeson hiding (Null)
import qualified Data.ByteString as LBS
import qualified Data.ByteString.Char8 as LBSC
data User =
     User {
      userId :: Maybe Integer,
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
    parseJSON (Object v) = User <$> v .:? "userId"
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
insertUser :: Query
insertUser =
    "INSERT INTO users VALUES (?,?,?,?,?,?)"

modifyUserSql ::  Query
modifyUserSql = [r|
                UPDATE users set
                username = ?,
                shell = ?,
                homeDirectory = ?,
                realName = ?,
                phone = ? where id = ?|]

modifyUser :: SQLite.Connection -> User  -> IO ()
modifyUser conn (User (Just userId_) username_ shell_ homeDirectory_ realName_ phone_) = do
   SQLite.execute conn modifyUserSql
       (username_,
        shell_,
        homeDirectory_,
        realName_,
        phone_, userId_)
   return ()
modifyUser _ _ = putStrLn "\nno user id supplied"

addUser :: SQLite.Connection -> User -> IO ()
addUser conn  =  SQLite.execute conn insertUser

processUser ::  SQLite.Connection -> User ->  IO()
processUser conn  user@(User (Just _) _ _ _ _ _) = modifyUser conn user
processUser conn  user@(User _ _ _ _ _ _) = addUser conn user


youRow = "{\"phone\":\"12324\",\"realName\":\"gregory nwosu\",\"username\":\"gregory\",\"shell\":\"/bin/bash\",\"userId\":1,\"homeDirectory\":\"/home/greg\"}"

main' :: IO()
main' = do
   let u = (User (Just 1) "gregory" "/bin/bash" "/home/greg" "gregory nwosu" "12324" )
   print u
   print  (encode  u )
