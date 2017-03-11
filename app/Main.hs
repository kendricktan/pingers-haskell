{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

-- Data
data DeviceLog = DeviceLog { deviceId :: String, epoch :: Int } deriving (Show, Generic)

-- Make our custom type JSON serializable
instance DA.ToJSON DeviceLog
instance DA.FromJSON DeviceLog

-- Database access
fetchDevices :: MonadIO m => SS.Connection -> String -> m [DeviceLog]
fetchDevices conn xs = return [DeviceLog xs 10]

-- High level routing
routes :: SS.Connection -> WS.ScottyM ()
routes conn = do
    WS.get "/:uid/:date" $ do
        --date <- (show <$> WS.param "date") :: String
        tt <- fetchDevices conn "hello"
        WS.json tt
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
    --r <- SS.query_ conn "SELECT uid, epoch from devices" :: IO [DeviceLog]
    --mapM_ print r
    WS.scotty 8000 $ routes conn
