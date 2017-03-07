{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Web.Scotty

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Aeson (FromJSON, ToJSON)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           GHC.Generics


data Checklist = Checklist {
    checklistId :: Maybe Int
  , title :: String
  , checklistItems :: [ChecklistItem]
} deriving (Show, Generic)

instance FromRow Checklist where
    fromRow = Checklist <$> field <*> field <*> pure []
instance ToRow Checklist where
    toRow c = [toField $ title c]
instance ToJSON Checklist
instance FromJSON Checklist

data ChecklistItem = ChecklistItem {
    checklistItemId :: Maybe Int
  , itemText :: String
  , finished :: Bool
  , checklist :: Int
} deriving (Show, Generic)

instance FromRow ChecklistItem where
    fromRow = ChecklistItem <$> field <*> field <*> field <*> field
instance ToRow ChecklistItem where
    toRow i = [toField $ itemText i, toField $ finished i, toField $ checklist i]
instance ToJSON ChecklistItem
instance FromJSON ChecklistItem

server :: Connection -> ScottyM ()
server conn = do
    get "/checklists" $ do
        checklists <- liftIO (query_ conn "select id, title from checklists" :: IO [Checklist])
        checkWithItems <- liftIO (mapM (setArray conn) checklists)
        json checkWithItems
    post "/" $ do
        item <- jsonData :: ActionM ChecklistItem
        newItem <- liftIO (insertChecklist conn item)
        json newItem

insertChecklist :: Connection -> ChecklistItem -> IO ChecklistItem
insertChecklist conn item = do
    let insertQuery = "insert into checklistitems (name, finished, checklist) values (?, ?, ?)"
    execute conn insertQuery item
    id <- lastInsertRowId conn
    return item { checklistItemId = Just (fromIntegral id) }

setArray :: Connection -> Checklist -> IO Checklist
setArray conn check = do
    let queryText = "select id, name, finished, checklist from checklistitems where checklist = (?)"
    items <- liftIO (query conn queryText (Only $ checklistId check) :: IO [ChecklistItem])
    return check {checklistItems = items}

main :: IO ()
main = do
    conn <- open "database.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS checklists (id INTEGER PRIMARY KEY, title TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS checklistitems (id INTEGER PRIMARY KEY, name TEXT NOT NULL, finished BOOLEAN NOT NULL, checklist INTEGER NOT NULL)"
    scotty 8080 $ server conn
