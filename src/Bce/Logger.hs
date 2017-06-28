module Bce.Logger where

import System.IO.Unsafe    

import Data.IORef
import Data.Ord
import Data.Time
import Data.Time.Clock


data LogLevel = TRACE | DEBUG | INFO | WARNING | ERROR deriving (Eq)

instance Show LogLevel where
    show TRACE = "debug"
    show DEBUG = "debug"
    show INFO = "info"
    show WARNING = "warning"
    show ERROR = "erro"

levelToInt :: LogLevel -> Int
levelToInt TRACE = 60                                 
levelToInt DEBUG = 50
levelToInt INFO = 40
levelToInt WARNING = 30
levelToInt ERROR = 20

                   

instance Ord LogLevel where
    compare = comparing levelToInt

globalLogLevel :: IORef LogLevel              
globalLogLevel = unsafePerformIO $ newIORef INFO

setLogLevel :: LogLevel -> IO ()
setLogLevel newLevel = writeIORef globalLogLevel newLevel

getLogLevel :: IO LogLevel
getLogLevel = readIORef globalLogLevel
                         

logmsg :: LogLevel -> String -> IO ()
logmsg level message = do
  maxLevel <- getLogLevel
  if level <= maxLevel
  then do
    time <- getCurrentTime
    let formatted = "[" ++ show level ++ "]" ++ "[" ++ show time ++ "] " ++ message
    putStrLn formatted
  else return ()

logTrace = logmsg TRACE
logDebug = logmsg DEBUG
logInfo = logmsg INFO
logWarning = logmsg WARNING
logError = logmsg ERROR          
        
    
    
