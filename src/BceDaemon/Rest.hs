{-# LANGUAGE OverloadedStrings #-}
module Rest where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import Control.Concurrent

data MySession = EmptySession
data MyAppState = AppState 


start port = forkIO $ restMain port

restMain :: Int -> IO ()
restMain port = do
              spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState)
              runSpock port (spock spockCfg app)
              return ()

app :: SpockM () MySession MyAppState ()
app = do
  get root $ text "hello"
  
