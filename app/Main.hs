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

data TestField = TestField Int T.Text deriving (Show, Generic)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance ToRow TestField where
    toRow (TestField id_ str) = toRow (id_, str)

instance ToJSON TestField

instance FromJSON TestField

server :: Connection -> ScottyM ()
server conn = get "/" $ do
        result <- liftIO (query_ conn "select id, title from pingers" :: IO [TestField])
        json result

main :: IO ()
main = do
    conn <- open "database.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS pingers (id INTEGER PRIMARY KEY, title TEXT)"
    scotty 8080 $ server conn
