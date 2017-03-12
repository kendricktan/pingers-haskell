{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Time
import Data.Time.Format
import Data.Time.Clock.POSIX
import Data.UnixTime
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

-- Time standards
understandTime :: String -> Int
understandTime s = (fromIntegral . round . utcTimeToPOSIXSeconds) $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s

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

-- Helper functions
slice :: Int -> Int -> String -> String
slice s e = take (e - s + 1) . drop s

str2epoch :: String -> Int
str2epoch s
  | '-' `elem` s = understandTime s
  | otherwise = read s :: Int

-- IO functions
getDevices :: SS.Connection -> String -> IO [DeviceLog]
getDevices conn uid = SS.query conn "SELECT uid, epoch from devices where uid = ?" (uid :: String) :: IO [DeviceLog]

getDevicesDate :: SS.Connection -> String -> Int -> Int -> IO [DeviceLog]
getDevicesDate conn uid st et = SS.query conn "SELECT uid, epoch from devices where uid = ? and epoch >= ? and epoch <= ?" (uid :: String, st :: Int, et :: Int) :: IO [DeviceLog]

insertDevice :: SS.Connection -> String -> Int -> IO ()
insertDevice c u e = SS.execute c "INSERT INTO devices (uid, epoch) VALUES (?, ?)" (DeviceLog u e)

-- High level routing
routes :: SS.Connection -> WS.ScottyM ()
routes conn = do
    WS.post "/:uid/:date" $ do
        uid <- WS.param "uid"
        date <- WS.param "date"
        liftIO $ insertDevice conn uid (str2epoch date)
        WS.text "inserted!"
    WS.get "/:uid/:date" $ do
        uid <- WS.param "uid"
        date <- WS.param "date"
        devices <- liftIO $ getDevicesDate conn uid (str2epoch date) (str2epoch date + 86400)
        WS.json devices
    WS.get "/:uid/:ftime/:ttime" $ do
        uid   <- WS.param "uid"
        ftime <- WS.param "ftime"
        ttime <- WS.param "ttime"
        devices <- liftIO $ getDevicesDate conn uid (str2epoch ftime) (str2epoch ttime)
        WS.json devices
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
