{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text                          as T
import qualified Database.SQLite.Simple             as SS
import qualified Database.SQLite.Simple.FromRow     as SF
import qualified Data.Aeson                         as DA
import qualified Web.Scotty                         as WS

-- Data type
data DeviceLog = DeviceLog { deviceId :: String, epoch :: Int } deriving (Show, Generic)

-- Make our custom type readable/writable from/to DB
instance SF.FromRow DeviceLog where
    fromRow = DeviceLog <$> SF.field <*> SF.field

instance SS.ToRow DeviceLog where
    toRow (DeviceLog i e) = SS.toRow (i, e)

-- Make our custom type JSON serializable
instance DA.ToJSON DeviceLog
instance DA.FromJSON DeviceLog

-- Helper function
getDevices :: SS.Connection -> IO String
getDevices conn = do
    d <- SS.query_ conn "SELECT uid, epoch from devices" :: IO [DeviceLog]
    return $ show d

-- High level routing
routes :: SS.Connection -> WS.ScottyM ()
routes conn = do
    WS.get "/:uid/:date" $ do
        date <- WS.param "date"
        r <- liftIO $ getDevices conn
        WS.text (date <> "!")
    WS.get "/:uid/:ftime/:ttime" $ do
        uid   <- WS.param "uid"
        ftime <- WS.param "ftime"
        ttime <- WS.param "ttime"
        WS.text (uid <> " " <> ftime <> " - " <> ttime <>"!")
    WS.post "/hello" $ WS.text "post world"

-- Main loop
main :: IO ()
main = do
    conn <- SS.open "database.db"
    SS.execute_ conn "CREATE TABLE IF NOT EXISTS devices (id INTEGER PRIMARY KEY, uid TEXT, epoch INTEGER)"
    --SS.execute conn "INSERT INTO devices (uid, epoch) VALUES (?, ?)" (DeviceLog "abdef" 456)
    --r <- SS.query_ conn "SELECT uid, epoch from devices" :: IO [DeviceLog]
    --mapM_ print r
    WS.scotty 8000 $ routes conn
