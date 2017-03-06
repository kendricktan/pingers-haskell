{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Aeson (FromJSON, ToJSON)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           GHC.Generics
import           Web.Scotty

data TestField = TestField { testFieldId :: Int
                           , title :: T.Text
                           } deriving (Show, Generic)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance ToRow TestField where
    toRow (TestField id_ str) = toRow (Only str)

instance ToJSON TestField

instance FromJSON TestField

server :: Connection -> ScottyM ()
server conn = do
    get "/" $ do
        result <- liftIO (query_ conn "select id, title from pingers" :: IO [TestField])
        json result
    post "/" $ do
        item <- jsonData :: ActionM TestField
        newItem <- liftIO (insertTestField conn item)
        json newItem

insertTestField :: Connection -> TestField -> IO TestField
insertTestField conn item = do
    let insertQuery = "insert into pingers (title) values (?)"
    execute conn insertQuery item
    id <- lastInsertRowId conn
    return item { testFieldId = fromIntegral id }

main :: IO ()
main = do
    conn <- open "database.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS pingers (id INTEGER PRIMARY KEY, title TEXT)"
    scotty 8080 $ server conn
