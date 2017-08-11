{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

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
import System.Console.Haskeline

import System.Console.CmdArgs


data ClientConfig = ClientConfig {
      backendAddress :: String
    , walletsDirectory :: String
    , nonInteractive :: Bool
    } deriving (Show, Data, Typeable)


logd msg = return () --putStrLn $ "[d]" ++ msg
logi msg = putStrLn $ "[i] " ++ msg
logw msg = putStrLn $ "[!] " ++ msg
loge msg = putStrLn $ "[!!!] " ++ msg
logs msg = putStrLn $ "[v] " ++ msg

shellPrefix = "[_]>> "

pubKeyLengthInHex = 32*2

type Error = String


loadKeyPair :: WalletId -> ClientConfig -> IO (Either Error KeyPair)
loadKeyPair walletId config = do
  let path = walletsDirectory config  ++ "/" ++ show walletId ++ ".kp"
  content <- BS.readFile path
  let kp = BinGet.runGet Bin.get (BSL.fromStrict content)  :: KeyPair
  return $ Right kp


processCmd :: Command -> ClientConfig -> IO ()
processCmd Shell _ = return ()
processCmd CreateWallet config = do
    liftIO $ createDirectoryIfMissing True $ walletsDirectory config
    g <- newGenIO :: IO CtrDRBG
    case generatePair g of
      Left err -> loge $ "failed to generate keypair: " ++ show err
      Right (kp, _) -> do
        logi "successfully generated keypair, writing to disk..."
        let path = walletsDirectory config ++ "/" ++ (show $ keyPairPub kp) ++ ".kp"
        let content = BSL.toStrict $ BinPut.runPut $ Bin.put kp                               
        BS.writeFile path content
        logs $ "successfully generated " ++ path

    
processCmd ListWallets config = do
  fs <- listDirectory $ walletsDirectory config
  let kpfs = filter (\p -> and [ length p == pubKeyLengthInHex+3
                              , isSuffixOf ".kp" p]) fs
  mapM_ (\(idx, kpf) ->
            logs $ "[" ++ show idx ++ "]" ++ " -> " ++ (take pubKeyLengthInHex $ kpf)
       ) $ zip [0..] kpfs


processCmd (PerformTransaction sender receiver amount) config = do
  msg <- runEitherT $ do
           liftIO $ logi "loading wallet..."
           kp <- EitherT $ loadKeyPair sender config
           liftIO $ logs $ "loaded wallet" ++ show (keyPairPub kp)
           liftIO $ logi "gathering information from node..."
           (WalletBalance checkedOutputRefs uncheckedOutputRefs) <- EitherT $ resolveBalance sender config
           let outputRefs = Set.difference checkedOutputRefs uncheckedOutputRefs
           pureOutputs <- EitherT (resolveOutputs (Set.toList outputRefs) config)
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
           ireq <- liftIO $ parseRequest $ "http://" ++ backendAddress config ++ "/transaction"
           let req =  ireq {method = "POST"
                           , requestBody = RequestBodyLBS (encode tx)}
           res <- liftIO $ httpLbs req manager
           liftIO $ logs $  "pushed transaction to server with response: " ++ (show $ responseBody res)
           right "done"

  case msg of
    Right s -> logs s
    Left e -> logw e

        

processCmd ShowHead config = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress config ++ "/head"
  res <- httpLbs req manager
  case decode $ responseBody res of
    Just (Head headLength headBlockId) -> do
        logs $ "head length:" ++ show headLength
        logs $ "head block id: " ++ show headBlockId
    Nothing -> loge "invalid format supplied" 
  
processCmd (ShowBlock blockId) config = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress config ++ "/block?blockId=" ++ show blockId
  res <- httpLbs req manager
  case decode $ responseBody res of
    Just blk@(RestBlock header transactions) -> do
--        logs $ "blockId:" ++ show (blockId header)
        logs $ "header: " ++ show header
        logs $ "transactions: "
        mapM_ (\tx -> putStrLn $  "  " ++ show tx) transactions
    Nothing -> loge "invalid format supplied" 

processCmd (ShowTransaction txId) config = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress config ++ "/transaction?txId=" ++ show txId             
  res <- httpLbs req manager
  case decode $ responseBody res of
    Just tx@(Transaction _ _ _) -> do
        logs $ "id: " ++ show txId
        logs $ show tx
    Just tx@(CoinbaseTransaction _) -> do
        logs $ "id: " ++ show txId
        logs $ show tx             
    Nothing -> loge "invalid format supplied" 


processCmd (QueryBalance walletId) config = do
  walletOpt <- resolveBalance walletId config
  case walletOpt of
    Right (WalletBalance outputRefs unconfirmedOutputRefs) -> do
          logs $ "total outputs: " ++ show (Set.size outputRefs)
          logi $ "resolving outputs... "
          outputsEthr <- resolveOutputs (Set.toList outputRefs) config
          case outputsEthr of
            Left err -> loge $ "failed to resolve outputs: "  ++ err
            Right outputs -> do
                     logs $ "in total: "  ++ show (sum $ map outputAmount outputs)
    Left err -> logw $ "failed to query balance: " ++ err


resolveBalance :: WalletId -> ClientConfig -> IO (Either Error WalletBalance)
resolveBalance walletId config = do
  manager <- newManager defaultManagerSettings  
  req <- parseRequest $ "http://" ++ backendAddress config ++ "/balance?wallet=" ++ show walletId
  res <-  httpLbs req manager
  let body = responseBody res
  logi $ "received response with length: " ++ show (BSL.length body)
  case decode body of
    Just (WalletBalance outputRefs unconfirmedOutputRefs) -> return $ Right
                                                             $ WalletBalance outputRefs unconfirmedOutputRefs
    Nothing -> return $ Left "invalid format"

resolveOutputs :: [TxOutputRef] -> ClientConfig -> IO (Either Error [TxOutput])
resolveOutputs refs config = do
    outs <- mapConcurrently (\r -> resolveOutput r config) refs 
    return $ sequence outs


resolveOutput :: TxOutputRef -> ClientConfig -> IO (Either Error TxOutput)
resolveOutput (TxOutputRef txId outputIdx) config = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ "http://" ++ backendAddress config ++ "/transaction?txId=" ++ show txId
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

clientConfig = ClientConfig { backendAddress = "127.0.0.1:8081" &= help "backend Address",
                              walletsDirectory = ".bcewallets/" &= help "wallets Directory" }
                            &= summary "Bce Cli v1"

getConfig :: IO ClientConfig
getConfig = cmdArgs clientConfig

repl cfg = runInputT defaultSettings loop
       where
         loop :: InputT IO ()
         loop  = do
           minput <- getInputLine shellPrefix
           case minput of
             Nothing -> loop
             Just cmdline -> do
               case parseCommand cmdline of
                 Left err -> do
                         liftIO $ loge $ "wrong input: " ++ show err
                 Right cmd -> do
                         liftIO $ processCmd cmd cfg
           loop


main = repl =<< getConfig
