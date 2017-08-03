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


import Data.Ord    
import System.IO
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.Trans    
import System.Directory
import System.Random
import Data.Aeson
import Data.Either
import Data.Either.Utils    
import Data.List    
import Crypto.Random.DRBG
import Control.Concurrent.Async    
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

type Error = String                    


loadKeyPair :: WalletId -> IO (Either Error KeyPair)
loadKeyPair walletId = do
  let path = walletsDirectory ++ "/" ++ show walletId ++ ".kp"
  content <- BS.readFile path
  let kp = BinGet.runGet Bin.get (BSL.fromStrict content)  :: KeyPair
  return $ Right kp


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
  msg <- runEitherT $ do
           liftIO $ logi "loading wallet..."
           kp <- EitherT $ loadKeyPair sender
           liftIO $ logs $ "loaded wallet" ++ show (keyPairPub kp)
           liftIO $ logi "gathering information from node..."
           (WalletBalance outputRefs) <- EitherT $ resolveBalance sender
           pureOutputs <- EitherT (resolveOutputs $ Set.toList outputRefs)
           let outputs = zip (Set.toList outputRefs) pureOutputs
           liftIO $ logs "got all needed information from node"

           liftIO $ logi "building transaction..."                
           let splits = inits outputs
           let splitsum s = sum $ map (fromIntegral . outputAmount . snd) s
           guard (amount <= splitsum outputs) `mplus` left "insufficient funds"
           let Just split = find (\s -> splitsum s >= amount) splits
           let change = splitsum split - amount
           let inputs = Set.fromList $ map (TxInput . fst) split
           let outputs = Set.union changeOutput receiverOutputs
                         where changeOutput =
                                   if change == 0
                                   then Set.empty
                                   else Set.singleton (TxOutput (fromIntegral change) $ keyPairPub kp)
                               receiverOutputs = Set.singleton $ TxOutput (fromIntegral amount) receiver
           let signature = sign (hash (inputs, outputs)) $ keyPairPriv kp
           let tx = Transaction inputs outputs signature
           liftIO $ logs "built transaction"

           liftIO $ logi "pushing transaction to server..."
           manager <- liftIO $ newManager defaultManagerSettings
           ireq <- liftIO $ parseRequest $ "http://" ++ backendAddress ++ "/transaction"
           let req =  ireq {method = "POST"
                           , requestBody = RequestBodyLBS (encode tx)}
           res <- liftIO $ httpLbs req manager
           liftIO $ logs $  "pushed transaction to server with response: " ++ (show $ responseBody res)
           right "done"

  case msg of
    Right s -> logs s
    Left e -> logw e

        

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
    Right (WalletBalance outputRefs) -> do
          logs $ "total outputs: " ++ show (Set.size outputRefs)
          logi $ "resolving outputs... "
          outputsEthr <- resolveOutputs $ Set.toList outputRefs
          case outputsEthr of
            Left err -> loge $ "failed to resolve outputs: "  ++ err
            Right outputs -> do
                     logs $ "in total: "  ++ show (sum $ map outputAmount outputs)
    Left err -> logw $ "failed to query balance: " ++ err


resolveBalance :: WalletId -> IO (Either Error WalletBalance)
resolveBalance walletId = do
  manager <- newManager defaultManagerSettings  
  req <- parseRequest $ "http://" ++ backendAddress ++ "/balance?wallet=" ++ show walletId
  res <-  httpLbs req manager
  let body = responseBody res
  logi $ "received response with length: " ++ show (BSL.length body)
  case decode body of
    Just (WalletBalance outputRefs) -> return $ Right $ WalletBalance outputRefs
    Nothing -> return $ Left "invalid format"

resolveOutputs :: [TxOutputRef] -> IO (Either Error [TxOutput])
resolveOutputs refs = do
    outs <- mapConcurrently resolveOutput refs
    return $ sequence outs


resolveOutput :: TxOutputRef -> IO (Either Error TxOutput)
resolveOutput (TxOutputRef txId outputIdx) = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress ++ "/transaction?txId=" ++ show txId
  res <- httpLbs req manager
  let body = responseBody res
--  logi $ show body
  case decode body of
    Just (Transaction txInputs txOutputs sig) ->
        return $ maybeToEither "output with such idx is missing"
                   ((Set.toList txOutputs) `at` (fromIntegral outputIdx))
    Just (CoinbaseTransaction txOutputs) ->
        return $ maybeToEither "output with such idx is missing"
                   ((Set.toList txOutputs) `at` (fromIntegral outputIdx))
    Nothing -> do
      logw "incorrect response format"
      return $ Left "incorrect response format"
              
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
                        

         
