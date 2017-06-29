{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Rest where

import Bce.BlockChain
import qualified Bce.DbFs as Db
import Bce.Util    
    

import Web.Spock
import Web.Spock.Config
import GHC.Generics    
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)    
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Set as Set

import Control.Concurrent

data ApiState = ApiState {apiDb :: Db.Db}  
type Api = SpockM () () ApiState ()
type ApiAction a = SpockAction () () ApiState a


start :: Db.Db -> Int -> IO ()
start db port =  discardResult $ forkIO $ restMain db port


restMain :: Db.Db -> Int -> IO ()
restMain db port = do
  let initialState = ApiState db
  spockCfg <- defaultSpockCfg () PCNoDatabase initialState
  runSpock port (spock spockCfg app)
  return ()

data ApiResponse = RespondError String
                 | RespondOk deriving (Generic)
instance FromJSON ApiResponse
instance ToJSON ApiResponse    


postTransaction = do
  tx <- jsonBody' :: ApiAction Transaction
  ApiState db <- getState
  res <- liftIO $ Db.pushTransactions db $ Set.singleton tx
  case res of
    Right () -> json RespondOk
    Left err -> json $ RespondError err


app :: Api
app = do
  get root $ text "welcome to BCE rest api"
  post "transaction" $ postTransaction

  
