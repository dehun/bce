module Main where

import Bce.BlockChain
import qualified Bce.P2p as P2p    
import qualified Bce.Networking as Networking
import qualified Bce.Miner as Miner

import qualified Bce.DbFs as Db     
import Bce.InitialBlock
    
    
import Control.Monad
import Control.Concurrent
import System.Environment

-- ./dist/build/bce/bce "(127,0,0,1)" 3555 "(127,0,0,1)" 3777 
main :: IO ()
main = do
  db <- Db.initDb "./tmpdb"
  Db.loadDb db
  [bindAddress, bindPort, seedAddress, seedPort] <- getArgs
  let seed = P2p.PeerAddress seedAddress (read seedPort)
  let p2pConfig = P2p.P2pConfig (P2p.PeerAddress bindAddress (read bindPort)) 5 5 1 25
  net <- Networking.start p2pConfig [seed] db
  let networkTimer = Networking.networkTime net
  putStrLn "started networking, starting up miner"
  Miner.growChain db networkTimer

  
