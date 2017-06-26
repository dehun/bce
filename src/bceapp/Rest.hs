module Rest where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import Control.Concurrent


start = forkIO $ restMain

restMain :: IO () = do
              spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState)
  
