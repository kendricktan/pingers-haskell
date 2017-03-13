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
data Pings = Pings [Int] deriving (Show, Generic)
data DeviceLog = DeviceLog String Int deriving (Show, Generic)
data DeviceIds = DeviceIds String deriving (Show, Generic)
data DevicePings = DevicePings String Pings deriving (Show, Generic)

-- Make our custom type readable/writable from/to DB
instance SF.FromRow DeviceLog where
    fromRow = DeviceLog <$> SF.field <*> SF.field

instance SS.ToRow DeviceLog where
    toRow (DeviceLog i e) = SS.toRow (i, e)

instance SF.FromRow (DevicePings String (Pings [Int])) where
    fromRow = DevicePings <$> SF.field <*> SF.field

instance SF.FromRow DeviceIds where
    fromRow = DeviceIds <$> SF.field

instance SF.FromRow Pings where
    fromRow = Pings <$> SF.field

-- Make our custom type JSON serializable
instance DA.ToJSON DeviceLog
instance DA.FromJSON DeviceLog

instance DA.ToJSON DevicePings
instance DA.FromJSON DevicePings

instance DA.ToJSON DeviceIds
instance DA.FromJSON DeviceIds

instance DA.ToJSON Pings
instance DA.FromJSON Pings

-- Helper functions
slice :: Int -> Int -> String -> String
slice s e = take (e - s + 1) . drop s

str2epoch :: String -> Int
str2epoch s
  | '-' `elem` s = understandTime s
  | otherwise = read s :: Int

-- IO functions
allDevices :: SS.Connection -> IO [DeviceIds]
allDevices conn = SS.query_ conn "SELECT uid from devices" :: IO [DeviceIds]

fromDates :: SS.Connection -> Int -> Int -> IO [DeviceLog]
fromDates conn ft tt = SS.query conn "SELECT uid, epoch from devices where epoch >= ? and epoch <= ?" (ft :: Int, tt :: Int) :: IO [DeviceLog]

fromDevicesAndDate :: SS.Connection -> String -> Int -> Int -> IO [DeviceLog]
fromDevicesAndDate conn uid ft tt = SS.query conn "SELECT uid, epoch from devices where uid = ? and epoch >= ? and epoch <= ?" (uid :: String, ft :: Int, tt :: Int) :: IO [DeviceLog]

insertDevice :: SS.Connection -> String -> Int -> IO ()
insertDevice c u e = SS.execute c "INSERT INTO devices (uid, epoch) VALUES (?, ?)" (DeviceLog u e)

clearDevices :: SS.Connection -> IO ()
clearDevices c = SS.execute_ c "DELETE FROM devices"

-- High level routing
routes :: SS.Connection -> WS.ScottyM ()
routes conn = do
    WS.get "/devices" $ do
        devices <- liftIO $ allDevices conn
        WS.json devices
    WS.get "/all/:date" $ do
        date <- WS.param "date"
        devices <- liftIO $ fromDates conn (str2epoch date) (str2epoch date + 86400)
        WS.json devices
    WS.get "/:uid/:date" $ do
        uid <- WS.param "uid"
        date <- WS.param "date"
        devices <- liftIO $ fromDevicesAndDate conn uid (str2epoch date) (str2epoch date + 86400)
        WS.json devices
    WS.get "/:uid/:ftime/:ttime" $ do
        uid   <- WS.param "uid"
        ftime <- WS.param "ftime"
        ttime <- WS.param "ttime"
        devices <- liftIO $ fromDevicesAndDate conn uid (str2epoch ftime) (str2epoch ttime)
        WS.json devices
    WS.post "/:uid/:date" $ do
        uid <- WS.param "uid"
        date <- WS.param "date"
        liftIO $ insertDevice conn uid (str2epoch date)
        WS.text "inserted!"
    WS.post "/clear_data" $ do
        liftIO $ clearDevices conn
        WS.text "cleared!"

-- Main loop
main :: IO ()
main = do
    conn <- SS.open "database.db"
    SS.execute_ conn "CREATE TABLE IF NOT EXISTS devices (id INTEGER PRIMARY KEY, uid TEXT, epoch INTEGER)"
    WS.scotty 8000 $ routes conn
