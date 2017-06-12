module Bce.TimeStamp where

import GHC.Int (Int64)
import Data.Time.Clock
import Data.Time.Clock.POSIX    

type TimeStamp = Int64

now :: IO Int64
now = round <$> getPOSIXTime
    
