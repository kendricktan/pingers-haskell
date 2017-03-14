{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Database
import Time
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map                           as M
import qualified Database.SQLite.Simple             as SS
import qualified Web.Scotty                         as WS

-- Remove duplicate Ids
removeDups :: Eq a => [a] -> [a]
removeDups = foldl (\seen x -> if x `elem` seen
                                  then seen
                                  else seen ++ [x]) []

-- Constructs a HashMap from an Array of DevicePings
toHashMap :: [DevicePings] -> M.Map String [Pings] -> M.Map String [Pings]
toHashMap [] m = m
toHashMap (DevicePings s a : xs) m = toHashMap xs (M.insert s a m)

-- High level routing
routes :: SS.Connection -> WS.ScottyM ()
routes conn = do
    WS.get "/devices" $ do
        devices <- liftIO $ allDevices conn
        WS.json (removeDups devices)
    WS.get "/all/:date" $ do
        date <- WS.param "date"
        d_devices <- liftIO $ allFromDates conn
        devices <- liftIO $ mapM (setEpochArray conn (str2epoch date) (str2epoch date + 86400)) (removeDups d_devices)
        WS.json (toHashMap devices M.empty)
    WS.get "/all/:ftime/:ttime" $ do
        ftime <- WS.param "ftime"
        ttime <- WS.param "ttime"
        d_devices <- liftIO $ allFromDates conn
        devices <- liftIO $ mapM (setEpochArray conn (str2epoch ftime) (str2epoch ttime)) (removeDups d_devices)
        WS.json (toHashMap devices M.empty)
    WS.get "/:uid/:date" $ do
        uid <- WS.param "uid"
        date <- WS.param "date"
        pings <- liftIO $ fromDevicesAndDate conn uid (str2epoch date) (str2epoch date + 86400)
        WS.json pings
    WS.get "/:uid/:ftime/:ttime" $ do
        uid   <- WS.param "uid"
        ftime <- WS.param "ftime"
        ttime <- WS.param "ttime"
        pings <- liftIO $ fromDevicesAndDate conn uid (str2epoch ftime) (str2epoch ttime)
        WS.json pings
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
