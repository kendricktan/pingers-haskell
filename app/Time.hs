module Time where

import Data.Time
import Data.Time.Format
import Data.Time.Clock.POSIX
import Data.UnixTime

-- Time standards
understandTime :: String -> Int
understandTime s = (fromIntegral . round . utcTimeToPOSIXSeconds) $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s

-- Helper functions
str2epoch :: String -> Int
str2epoch s
  | '-' `elem` s = understandTime s
  | otherwise = read s :: Int

