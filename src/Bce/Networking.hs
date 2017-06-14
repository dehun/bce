{-# LANGUAGE DeriveGeneric #-}

module Bce.Networking where

import qualified Bce.BlockChain as BlockChain
import qualified Bce.Db as Db
import qualified Bce.P2p as P2p
import Bce.Hash
import Bce.BlockChainHash
import Bce.Util
import Bce.TimeStamp    

import qualified Data.Binary as Bin
import GHC.Generics (Generic)
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative
import Data.Monoid
import Debug.Trace
import Control.Concurrent    
import Control.Concurrent.STM

    

type PeerAddress = P2p.PeerAddress    

data Network = Network { networkP2p :: P2p.P2p
                       , networkDb :: Db.Db }

data NetworkMessage = Brag Int
                    | Ask Hash
                    | Propose [BlockChain.Block]
                    | Dunno Hash
                    | PushTransactions [BlockChain.Transaction]
                      deriving (Show, Generic)

-- serialization                               
instance Bin.Binary Hash
instance Bin.Binary BlockChain.TxOutputRef                    
instance Bin.Binary BlockChain.TxOutput                
instance Bin.Binary BlockChain.TxInput            
instance Bin.Binary BlockChain.Transaction        
instance Bin.Binary BlockChain.BlockHeader    
instance Bin.Binary BlockChain.Block    
instance Bin.Binary NetworkMessage

encodeMessage :: NetworkMessage -> BS.ByteString
encodeMessage msg = BSL.toStrict $ BinPut.runPut $ Bin.put msg

decodeMessage :: BS.ByteString -> NetworkMessage
decodeMessage bs = BinGet.runGet Bin.get $ BSL.fromStrict bs

--

handlePeerMessage :: Network -> PeerAddress -> NetworkMessage -> IO ()
handlePeerMessage net peer msg = do
    let db = networkDb net
--    putStrLn $ "got message " ++ show msg
    case msg of
      Brag braggedLen -> do
--               putStrLn $ "saw bragger with length!" ++ show braggedLen
               (dbLen, topBlock) <- atomically $ do
                                      (,) <$> (Db.getChainLength db)
                                              <*> (Db.getTopBlock db)
               if dbLen < braggedLen
               then send net peer $ Ask $ hash topBlock
               else return ()
      Ask fromHash -> do
               blocksOpt <- atomically $ Db.blocksFromHash (networkDb net) fromHash
               case blocksOpt of
                 Just blocks -> send net peer $ Propose blocks
                 Nothing -> send net peer $ Dunno fromHash
      Propose blocks -> do
               discardResult $ atomically $ Db.regrowChain (networkDb net) blocks
      Dunno fromHash -> do
               prevBlockOpt <- atomically $  Db.getBlock db fromHash
               case prevBlockOpt of
                 Just prevBlock -> send net peer $ Ask
                                   $ (BlockChain.bhPrevBlockHeaderHash $ BlockChain.blockHeader prevBlock)
                 Nothing -> return ()
      PushTransactions transactions -> do
        atomically $ Db.pushTransactions (networkDb net) transactions
        return ()

networkListener :: Network -> IO ()
networkListener net = do
      chan <- atomically $ dupTChan $ P2p.p2pRecvChan $ networkP2p net
      let loop = do
            msg <- atomically $ readTChan chan
            case msg of
              P2p.PeerDisconnected peer -> do
                        return ()
              P2p.PeerMessage peer bs -> do
--                  putStrLn $ "got message of length " ++ (show (BS.length bs))
                  handlePeerMessage net peer $ decodeMessage bs
            loop
      loop

bragger :: Network -> IO ()
bragger net =
    let loop = do
          threadDelay (secondsToMicroseconds $ 5)
          length <- atomically $ Db.getChainLength (networkDb net)
--          putStrLn $ "bragging with length of " ++ show length
          broadcast net $ Brag length
          loop
    in loop

transactionsAnnouncer :: Network -> IO ()
transactionsAnnouncer net = do
  threadDelay (secondsToMicroseconds $ 5)
  transactionsToAnnounce <- atomically $ (Db.getTransactions $ networkDb net)
  broadcast net $ PushTransactions transactionsToAnnounce
  transactionsAnnouncer net
              

start :: P2p.P2pConfig -> [P2p.PeerAddress] -> Db.Db -> IO Network 
start p2pConfig seeds db  = do
    network <- Network <$> (P2p.start seeds p2pConfig) <*> pure db
    forkIO $ networkListener network
    forkIO $ bragger network
    forkIO $ transactionsAnnouncer network
    return network

broadcast :: Network -> NetworkMessage -> IO ()
broadcast net msg = P2p.broadcastPayload (networkP2p net) (encodeMessage msg)

send :: Network -> PeerAddress -> NetworkMessage -> IO ()
send net peer msg = P2p.sendPayload  (networkP2p net) peer (encodeMessage msg)

networkTime :: Network -> IO TimeStamp
networkTime net = P2p.networkTime $ networkP2p net
