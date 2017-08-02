{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Control.Monad
import Data.Either    

import Command
import Bce.Crypto
import Bce.Hash    
import Bce.Util    
import Bce.BlockChainSerialization
import Bce.RestTypes
import Bce.BlockChain


import System.IO
import System.Directory
import System.Random
import Data.Aeson
import Data.List    
import Crypto.Random.DRBG
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Set as Set
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


backendAddress = "127.0.0.1:8081"
walletsDirectory = ".bcewallets/"    

logd msg = return () --putStrLn $ "[d]" ++ msg
logi msg = putStrLn $ "[i] " ++ msg
logw msg = putStrLn $ "[!] " ++ msg
loge msg = putStrLn $ "[!!!] " ++ msg
logs msg = putStrLn $ "[v] " ++ msg

shellPrefix = "[_]>> "

pubKeyLengthInHex = 32*2

processCmd Shell = return ()
processCmd CreateWallet = do
    createDirectoryIfMissing True walletsDirectory
    g <- newGenIO :: IO CtrDRBG
    case generatePair g of
      Left err -> loge $ "failed to generate keypair: " ++ show err
      Right (kp, _) -> do
        logi "successfully generated keypair, writing to disk..."
        let path = walletsDirectory ++ "/" ++ (show $ keyPairPub kp) ++ ".kp"
        let content = BSL.toStrict $ BinPut.runPut $ Bin.put kp                               
        BS.writeFile path content
        logs $ "successfully generated " ++ path
    
processCmd ListWallets = do
  fs <- listDirectory walletsDirectory
  let kpfs = filter (\p -> and [ length p == pubKeyLengthInHex+3
                              , isSuffixOf ".kp" p]) fs
  mapM_ (\(idx, kpf) ->
            logs $ "[" ++ show idx ++ "]" ++ " -> " ++ (take pubKeyLengthInHex $ kpf)
       ) $ zip [0..] kpfs

processCmd (PerformTransaction sender receiver amount) = do
  manager <- newManager defaultManagerSettings  
  initialReq <- parseRequest "http://localhost:8081/transaction"
  undefined 
--  let tx = Transaction 
--  let req = initialReq {method = "POST", requestBody = RequestBodyLBS $ encode requestObject}

processCmd ShowHead = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress ++ "/head"
  res <- httpLbs req manager
  case decode $ responseBody res of
    Just (Head headLength headBlockId) -> do
        logs $ "head length:" ++ show headLength
        logs $ "head block id: " ++ show headBlockId
    Nothing -> loge "invalid format supplied" 
  
processCmd (ShowBlock blockId) = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress ++ "/block?blockId=" ++ show blockId
  res <- httpLbs req manager
  case decode $ responseBody res of
    Just blk@(RestBlock header transactions) -> do
--        logs $ "blockId:" ++ show (blockId header)
        logs $ "header: " ++ show header
        logs $ "transactions: "
        mapM_ (\tx -> putStrLn $  "  " ++ show tx) transactions
    Nothing -> loge "invalid format supplied" 

processCmd (ShowTransaction txId) = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress ++ "/transaction?txId=" ++ show txId             
  res <- httpLbs req manager
  case decode $ responseBody res of
    Just tx@(Transaction _ _ _) -> do
        logs $ "id: " ++ show txId
        logs $ show tx
    Just tx@(CoinbaseTransaction _) -> do
        logs $ "id: " ++ show txId
        logs $ show tx             
    Nothing -> loge "invalid format supplied" 


processCmd (QueryBalance walletId) = do
  walletOpt <- resolveBalance walletId
  case walletOpt of
    Just (WalletBalance outputRefs) -> do
          logs $ "total outputs: " ++ show (Set.size outputRefs)
          logi $ "resolving outputs... "
          outputsOpt <- mapM resolveOutput $ Set.toList outputRefs
          case sequence outputsOpt of
            Nothing -> loge "failed to resolve outputs"
            Just outputs -> do
                     logs $ "in total: "  ++ show (sum $ map outputAmount outputs)
    Nothing -> logw "wrong response format"


resolveBalance :: WalletId -> IO (Maybe WalletBalance)
resolveBalance walletId = do
  manager <- newManager defaultManagerSettings  
  req <- parseRequest $ "http://" ++ backendAddress ++ "/balance?wallet=" ++ show walletId
  res <-  httpLbs req manager
  let body = responseBody res
  logi $ "received response with length: " ++ show (BSL.length body)
  case decode body of
    Just (WalletBalance outputRefs) -> return $ Just $ WalletBalance outputRefs
    Nothing -> return Nothing


resolveOutput :: TxOutputRef -> IO (Maybe TxOutput)
resolveOutput (TxOutputRef txId outputIdx) = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress ++ "/transaction?txId=" ++ show txId
  res <- httpLbs req manager
  let body = responseBody res
  logi $ show body
  case decode body of
    Just (Transaction txInputs txOutputs sig) ->
        return $ (Set.toList txOutputs) `at` (fromIntegral outputIdx)
    Just (CoinbaseTransaction txOutputs) ->
        return $ (Set.toList txOutputs) `at` (fromIntegral outputIdx)
    Nothing -> do
      logw "incorrect response format"
      return Nothing
              
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
         putStr shellPrefix
         cmdLine <- getLine
         case parseCommand cmdLine of
           Left err -> do
              putStrLn $ "wrong input: " ++ show err
           Right cmd -> do
              processCmd cmd
                        

         
