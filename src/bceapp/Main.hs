module Main where

import Bce.BlockChain
import qualified Bce.P2p as P2p    
import qualified Bce.Db as Db    
import qualified Bce.Networking as Networking
import qualified Bce.Miner as Miner    

    
import Control.Monad
import Control.Concurrent
import System.Environment


main :: IO ()
main = do
  db <- Db.newDb  
  [bindAddress, bindPort, seedAddress, seedPort] <- getArgs
  let seed = P2p.PeerAddress seedAddress (read seedPort)
  let p2pConfig = P2p.P2pConfig (P2p.PeerAddress bindAddress (read bindPort)) 15 15
  net <- Networking.start p2pConfig [seed] db
  putStrLn "started networking, starting up miner"
  Miner.growChain db

  
