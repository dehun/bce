module Bce.TimeStamp where

import GHC.Int (Int64)
import Data.Time.Clock
import Data.Time.Clock.POSIX    

type TimeStamp = Int64

type Timer = IO TimeStamp    

now :: Timer
now = round <$> getPOSIXTime

      
    
