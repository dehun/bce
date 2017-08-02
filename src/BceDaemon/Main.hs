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

-- ./dist/build/bce/bcedaemon "(127,0,0,1)" 3555 "(127,0,0,1)" 3777 8080
main :: IO ()
main = do
  logInfo "starting up"
  [bindAddress, bindPort, seedAddress, seedPort, restApiPort] <- getArgs              
  logInfo "init db"            
  db <- Db.initDb "./tmpdb"
  logInfo "loading db data"            
  Db.loadDb db
  logInfo $ "starting up p2p networking at port " ++ bindPort
  let seed = P2p.PeerAddress seedAddress (read seedPort)
  let p2pConfig = P2p.P2pConfig (P2p.PeerAddress bindAddress (read bindPort)) 5 5 1 25
  net <- Networking.start p2pConfig [seed] db
  let networkTimer = Networking.networkTime net
  logInfo $ "starting http on port " ++ show restApiPort
  Rest.start db (read restApiPort)
  logInfo $ "starting mining"

  g <- newGenIO :: IO CtrDRBG

  let Right (keyPair, gNew) = generatePair g

  let ownerKey = keyPairPub keyPair
  logInfo $ "mining with key " ++ show ownerKey

  Miner.mineForever db ownerKey networkTimer

  
