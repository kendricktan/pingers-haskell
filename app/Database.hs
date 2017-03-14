{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import GHC.Generics
import Control.Monad.IO.Class (liftIO)

import qualified Database.SQLite.Simple             as SS
import qualified Database.SQLite.Simple.FromRow     as SF
import qualified Data.Aeson                         as DA

-- Data type
data Pings = Pings Int deriving (Show, Generic)
data DeviceLog = DeviceLog String Pings deriving (Show, Generic)
data DeviceId = DeviceId String deriving (Show, Generic)
data DevicePings = DevicePings String [Pings] deriving (Show, Generic)

-- Make Eq instance of our custom data types to enable folding
instance Eq DeviceId where
    (DeviceId a) == (DeviceId b) = a == b

instance Eq DevicePings where
    (DevicePings a _) == (DevicePings b _) = a == b

-- Make our custom type readable/writable from/to DB
instance SF.FromRow Pings where
    fromRow = Pings <$> SF.field

instance SF.FromRow DeviceLog where
    fromRow = DeviceLog <$> SF.field <*> SF.fromRow

instance SS.ToRow DeviceLog where
    toRow (DeviceLog i (Pings e)) = SS.toRow (i, e)

instance SF.FromRow DeviceId where
    fromRow = DeviceId <$> SF.field

instance SF.FromRow DevicePings where
    fromRow = DevicePings <$> SF.field <*> pure []

-- Make our custom type JSON serializable
instance DA.ToJSON DeviceLog
instance DA.FromJSON DeviceLog

instance DA.ToJSON DevicePings
instance DA.FromJSON DevicePings

instance DA.ToJSON DeviceId
instance DA.FromJSON DeviceId

instance DA.ToJSON Pings
instance DA.FromJSON Pings

-- IO functions
allDevices :: SS.Connection -> IO [DeviceId]
allDevices conn = SS.query_ conn "SELECT uid from devices" :: IO [DeviceId]

allFromDates :: SS.Connection -> IO [DevicePings]
allFromDates conn = SS.query_ conn "SELECT uid from devices" :: IO [DevicePings]

fromDevicesAndDate :: SS.Connection -> String -> Int -> Int -> IO [Pings]
fromDevicesAndDate conn uid ft tt = SS.query conn "SELECT epoch from devices where uid = ? and epoch >= ? and epoch < ?" (uid :: String, ft :: Int, tt :: Int) :: IO [Pings]

insertDevice :: SS.Connection -> String -> Int -> IO ()
insertDevice c u e = SS.execute c "INSERT INTO devices (uid, epoch) VALUES (?, ?)" (DeviceLog u (Pings e))

clearDevices :: SS.Connection -> IO ()
clearDevices c = SS.execute_ c "DELETE FROM devices"

-- Nest query results together
setEpochArray :: SS.Connection -> Int -> Int -> DevicePings -> IO DevicePings
setEpochArray conn ft tt (DevicePings uid _) = do
    pings <- liftIO $ SS.query conn "SELECT epoch FROM devices WHERE uid = ? and epoch >= ? and epoch < ?" (uid :: String, ft :: Int, tt :: Int) :: IO [Pings]
    return $ DevicePings uid pings

