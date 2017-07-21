{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Rest where

import Bce.BlockChain
import Bce.Verified    
import qualified Bce.DbFs as Db
import qualified Bce.VerifiedDb as VerifiedDb    
import Bce.Util
import Bce.Hash    
import Bce.Crypto
import Bce.Logger
    


import Web.Spock
import Web.Spock.Config
import GHC.Generics    
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)    
import Control.Monad.Trans
import Control.Concurrent    
import Data.Monoid
import Data.IORef
import qualified Data.Set as Set


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

data ApiResponse = RespondError { error :: String }
                 | RespondOk deriving (Generic)
instance FromJSON ApiResponse
instance ToJSON ApiResponse


getRoot = text "welcome to BCE rest api"

getBlock = do
  ApiState db <- getState
  blockIdStrOpt <- param "blockId"
  case blockIdStrOpt of
    Nothing -> json $ RespondError "no blockId parameter"
    Just blockIdStr -> do
          let blockId = read blockIdStr :: Hash
          blockOpt <- liftIO $ Db.getBlock db blockId
          case blockOpt of
            Just (VerifiedBlock block) -> json block
            Nothing -> json $ RespondError "no such block"

getTransaction = do
  ApiState db <- getState
  txIdStrOpt <- param "txId"
  case txIdStrOpt of
    Just txIdStr -> do
        let txId = read txIdStr :: Hash
        liftIO $ logInfo $ "looking up txId" ++ show txId
        txOpt <- liftIO $ Db.getDbTransaction db txId
        case txOpt of
          Just tx -> json tx
          Nothing -> json $ RespondError "no transaction with such id"
    Nothing ->
        json $ RespondError "no txId parameter"
  

postTransaction = do
  tx <- jsonBody' :: ApiAction Transaction
  ApiState db <- getState
  res <- liftIO $ VerifiedDb.verifyAndPushTransactions db $ Set.singleton tx
  case res of
    Right () -> json RespondOk
    Left err -> json $ RespondError err


data WalletBalance = WalletBalance { outputs :: Set.Set TxOutputRef } deriving (Show, Eq, Generic)
instance ToJSON WalletBalance                   


getBalance = do
  ApiState db <- getState  
  pubKeyStrOpt <- param "wallet"
  case pubKeyStrOpt of
    Just pubKeyStr -> do
        let pubKey = read pubKeyStr :: PubKey
        liftIO $ logInfo $ "inspecting wallet " ++ show pubKey
        balance <- liftIO $ Db.getPubKeyBalance db pubKey
        json $ WalletBalance balance
    Nothing -> json $ RespondError "no wallet parameter"


app :: Api
app = do
  get root $ getRoot
  get "/block" getBlock  
  get "/transaction" getTransaction
  post "/transaction" postTransaction
  get "/balance" getBalance

  
