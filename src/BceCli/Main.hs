import Control.Monad
import Data.Either    

import Command
import Bce.Crypto
import Bce.BlockChainSerialization
import Bce.RestTypes    


import System.IO
import System.Directory
import System.Random
import Data.Aeson
import Data.List    
import Crypto.Random.DRBG
import Network.HTTP.Client    
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

walletsDirectory = ".bcewallets/"    

logd msg = return () --putStrLn $ "[d]" ++ msg
logi msg = putStrLn $ "[i] " ++ msg
logw msg = putStrLn $ "[!] " ++ msg
loge msg = putStrLn $ "[!!!] " ++ msg
logs msg = putStrLn $ "[v]" ++ msg

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

processCmd ShowHead = undefined
processCmd (ShowBlock blockId) = undefined
processCmd (ShowTransaction txId) = undefined
processCmd (QueryBalance walletId) = do
  manager <- newManager defaultManagerSettings  
  req <- parseRequest $ "http://localhost:8081/balance?wallet=" ++ show walletId
  res <-  httpLbs req manager
  let body = responseBody res
  putStrLn $ show body
--  let WalletBalance outputs = parseJSON bs
  return ()
              
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
                        
         
