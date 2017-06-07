module Main where

import Bce.BlockChain
import qualified Bce.P2p as P2p
import Control.Monad
import Control.Concurrent    

main :: IO ()
main = do
  let p2pSeeds1 = [P2p.PeerAddress "(127,0,0,1)" 3666, P2p.PeerAddress "(127,0,0,1)" 3668]
  let p2pConfig1 = P2p.P2pConfig (P2p.PeerAddress "(127,0,0,1)" 3667) 5 5
  p2p <- P2p.start  p2pSeeds1 p2pConfig1
  let p2pSeeds2 = [P2p.PeerAddress "(127,0,0,1)" 3667]
  let p2pConfig2 = P2p.P2pConfig (P2p.PeerAddress "(127,0,0,1)" 3666) 5 5
  p2p2 <- P2p.start  p2pSeeds2 p2pConfig2
  putStrLn "p2pStarted"
  forever yield
