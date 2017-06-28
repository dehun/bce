{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Rest where

import Bce.BlockChain    

import Web.Spock
import Web.Spock.Config
import GHC.Generics    
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)    
import Control.Monad.Trans
import Data.Monoid
import Data.IORef

import Control.Concurrent


data MySession = EmptySession
type Api = SpockM () () () ()


type ApiAction a = SpockAction () () () a

data ExampleData = ExampleData {a :: Int, b ::Int, c::String }  deriving (Generic, Show)
instance ToJSON ExampleData
instance FromJSON ExampleData    


start port = forkIO $ restMain port

restMain :: Int -> IO ()
restMain port = do
              spockCfg <- defaultSpockCfg () PCNoDatabase ()
              runSpock port (spock spockCfg app)
              return ()


postTransaction = do
  thePerson <- jsonBody' :: ApiAction ExampleData
  text $ "Parsed: " <> pack (show thePerson)


app :: Api
app = do
  get root $ text "hello"
  post "transaction" $ do
        postTransaction

  
