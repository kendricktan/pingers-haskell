{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Control.Applicative

import qualified Database.SQLite.Simple             as SS
import qualified Database.SQLite.Simple.FromRow     as SF
import qualified Data.Aeson                         as DA
import qualified Web.Scotty                         as WS

-- Data
data DeviceLog = DeviceLog { deviceId :: String, epoch :: Int } deriving (Show, Generic)

-- Make our custom type JSON serializable
instance DA.ToJSON DeviceLog
instance DA.FromJSON DeviceLog

-- High level routing
routes :: WS.ScottyM ()
routes = do
    WS.get "/:uid/:date" $ do
        date <- WS.param "date"
        WS.text (date <> " !")
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
    WS.scotty 8000 routes
