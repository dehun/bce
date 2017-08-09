module Main where

import Rest
    
import Bce.BlockChain
import Bce.Crypto    
import Bce.Logger    
import qualified Bce.P2p as P2p    
import qualified Bce.Networking as Networking
import qualified Bce.Miner as Miner

import qualified Bce.DbFs as Db     
import Bce.InitialBlock

import Control.Monad
import Control.Concurrent
import System.Environment
import qualified Data.ByteString as BS    

import Crypto.Random.DRBG

data DaemonConfig = DaemonConfig {
      dataDirectory :: String
    , restBindAddress :: Networking.PeerAddress
    , ownerKey :: PubKey
    , p2pConfig :: P2p.P2pConfig
    , seedAddresses :: [Networking.PeerAddress]
      } deriving (Show, Eq)


defaultP2pConfig = P2p.P2pConfig (P2p.PeerAddress "(127,0,0,1)" 3666) 5 5 1 25                  
defaultDaemonConfig = DaemonConfig {
                        dataDirectory="./tmpdb"
                      , seedAddresses=[P2p.PeerAddress "(127,0,0,1)" 3555]
                      , p2pConfig = defaultP2pConfig
                      , restBindAddress=P2p.PeerAddress "(127,0,0,1)" 8081
                      , ownerKey = read "e9c003c3804bc84c1e0996fcb5279927c0cef0e4ed27c114abe86a17cf776eba" }

loadConfig :: IO (Either String DaemonConfig)
loadConfig = do
    [bindAddress, bindPort, seedAddress, seedPort, restApiPort, ownerKey] <- take 6 <$> getArgs              
    return $ Right $ defaultDaemonConfig { seedAddresses=[P2p.PeerAddress seedAddress (read seedPort)]
                                         , restBindAddress=P2p.PeerAddress "0.0.0.0" (read restApiPort)
                                         , ownerKey=read ownerKey
                                         , p2pConfig = defaultP2pConfig {
                                             P2p.p2pConfigBindAddress = P2p.PeerAddress bindAddress (read bindPort) } }

-- ./dist/build/bce/bcedaemon "(127,0,0,1)" 3555 "(127,0,0,1)" 3777 8080 e9c003c3804bc84c1e0996fcb5279927c0cef0e4ed27c114abe86a17cf776eba
main :: IO ()
main = do
  configE <- loadConfig
  case configE of
    Left err -> logError $ "fatal, invalid config: " ++ err
    Right config -> do
      logInfo "starting up"
      logInfo "init db"            
      db <- Db.initDb $ dataDirectory config
      logInfo "loading db data"            
      Db.loadDb db

      net <- Networking.start (p2pConfig config) (seedAddresses config) db
      let networkTimer = Networking.networkTime net
      logInfo $ "starting http on " ++ show (restBindAddress config)
      Rest.start db (fromIntegral $ P2p.peerPort $ restBindAddress config)
      logInfo $ "starting mining"

      logInfo $ "mining with key " ++ show  (ownerKey config)
      Miner.mineForever db (ownerKey config) networkTimer

  
