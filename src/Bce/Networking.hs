{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bce.Networking where

import qualified Bce.BlockChain as BlockChain
import qualified Bce.DbFs as Db
import qualified Bce.VerifiedDb as VerifiedDb
import qualified Bce.P2p as P2p
import Bce.Logger
import Bce.Verified    
import Bce.Hash
import Bce.BlockChainHash
import Bce.Util
import Bce.TimeStamp
import Bce.InitialBlock    
import Bce.BlockChainSerialization    

import qualified Data.Binary as Bin
import qualified Data.Set as Set
import qualified Data.Map as Map        
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
import Control.Monad.Trans
import Control.Monad.Trans.State
import Bce.PeerAddress



data NetworkState = NetworkState { networkP2p :: P2p.P2p
                                 , networkStateDb :: Db.Db }
data Network = Network { networkState :: NetworkState
                       , braggerThread :: ThreadId
                       , transactionAnnouncherThread :: ThreadId
                       , networkListenerThread :: ThreadId }

data NetworkMessage = Brag Int
                    | Ask Hash
                    | Propose [BlockChain.Block]
                    | Dunno
                    | PushTransactions (Set.Set BlockChain.Transaction)
                      deriving (Show, Generic)
instance Bin.Binary NetworkMessage    
                               

encodeMessage :: NetworkMessage -> BS.ByteString
encodeMessage msg = BSL.toStrict $ BinPut.runPut $ Bin.put msg

decodeMessage :: BS.ByteString -> NetworkMessage
decodeMessage bs = BinGet.runGet Bin.get $ BSL.fromStrict bs

maxBlocksSyncBatch = 50

handlePeerMessage :: NetworkState -> PeerAddress -> NetworkMessage -> StateT NetworkListenerState IO ()
handlePeerMessage net peer msg = do
    let db = networkStateDb net
    liftIO $ logDebug $ "got message " ++ show msg
    case msg of
      Brag braggedLen -> do
               (dbLen, VerifiedBlock topBlock) <- liftIO $ Db.getLongestHead db
               if dbLen < braggedLen
               then do
                 liftIO $ logDebug $ "saw bragger with length!" ++ show braggedLen
                 s <- get
                 if Map.member peer (networkListenerActiveSyncs s)
                 then liftIO $ logDebug "already syncing from that bragger"
                 else do
                   liftIO $ logDebug $  "starting sync from " ++ show peer
                   let askFrom = blockId topBlock
                   let newSync = NetworkBlocksSyncState 1 askFrom
                   let ns = s{networkListenerActiveSyncs=Map.insert peer newSync $ networkListenerActiveSyncs s}
                   put ns
                   liftIO $ isend net peer $ Ask askFrom
               else return ()
      Ask fromHash -> do
               blocksOpt <- liftIO $ Db.getBlocksFrom (networkStateDb net) fromHash
               case blocksOpt of
                 Just blocks -> do
                     liftIO $ logDebug $ "proposing blocks from " ++ show fromHash
                     liftIO $ isend net peer $ Propose $ blocks
                 Nothing -> do
                     liftIO $ logDebug $ "dunno from hash" ++ show fromHash
                     liftIO $ isend net peer $ Dunno
      Propose blocks -> do
               modify (\oldState -> oldState{networkListenerActiveSyncs=
                                                 Map.delete peer $ networkListenerActiveSyncs oldState})
               liftIO $ logDebug $ "pushing blocks to chain" ++ show (length blocks)
               liftIO $ VerifiedDb.verifyAndPushBlocks (networkStateDb net) blocks
      Dunno -> do
               oldState <- get
               let oldSyncStateOpt = Map.lookup peer $ networkListenerActiveSyncs oldState
               case oldSyncStateOpt of
                 Nothing -> liftIO $ logWarning $ "got dunno for unstarted sync from "  ++ show peer
                 Just oldSyncState -> do
                     let askSkipInterval = 2 ^ (networkBlocksSyncAccelerationKoef oldSyncState)
                     let oldFromHash = networkBlockSyncLastAsk oldSyncState
                     oldBlocksOpt <- liftIO $ Db.getBlocksTo db oldFromHash askSkipInterval
                     let newFromHash = case oldBlocksOpt of
                                         Nothing -> blockId initialBlock
                                         Just oldBlocks -> blockId $ verifiedBlock $ last oldBlocks
                     let newSyncState = oldSyncState{ networkBlocksSyncAccelerationKoef =
                                                         1 + networkBlocksSyncAccelerationKoef oldSyncState
                                                    , networkBlockSyncLastAsk = newFromHash }
                     put oldState{networkListenerActiveSyncs=Map.insert peer newSyncState
                                                              $ networkListenerActiveSyncs oldState}
                     liftIO $ logDebug $ "got dunno; asking from" ++ show newFromHash
                                     ++ " with skip " ++ show askSkipInterval
                     liftIO $ isend net peer $ Ask newFromHash
      PushTransactions transactions -> do
        liftIO $ VerifiedDb.verifyAndPushTransactions (networkStateDb net) transactions
        return ()


data NetworkBlocksSyncState = NetworkBlocksSyncState { networkBlocksSyncAccelerationKoef :: Int
                                                     , networkBlockSyncLastAsk :: Hash } deriving (Show)
data NetworkListenerState = NetworkListenerState {
      networkListenerActiveSyncs :: Map.Map PeerAddress NetworkBlocksSyncState } deriving (Show)


networkListener :: NetworkState -> IO ()
networkListener net = do
      chan <- atomically $ dupTChan $ P2p.p2pRecvChan $ networkP2p net
      let initialState = NetworkListenerState Map.empty
      let loop = do
            msg <- liftIO $ atomically $ readTChan chan
            case msg of
              P2p.PeerConnected peer -> do
                  liftIO $ Db.pushSeed peer
              P2p.PeerDisconnected peer -> do
                  liftIO $ logDebug $ "peer disconnected " ++ show peer
                  modify (\oldState -> oldState{networkListenerActiveSyncs=
                                                    Map.delete peer $ networkListenerActiveSyncs oldState})
              P2p.PeerMessage peer bs -> handlePeerMessage net peer $ decodeMessage bs
            loop 
      evalStateT loop initialState

bragger :: NetworkState -> IO ()
bragger net =
    let loop = do
          threadDelay (secondsToMicroseconds $ 5)
          (length, _) <- Db.getLongestHead (networkStateDb net)
          logTrace $ "bragging with length of " ++ show length
          ibroadcast net $ Brag length
          loop
    in loop

transactionsAnnouncer :: NetworkState -> IO ()
transactionsAnnouncer net = do
  threadDelay (secondsToMicroseconds $ 5)
  transactionsToAnnounce <- Db.getTransactions $ networkStateDb net
  ibroadcast net $ PushTransactions transactionsToAnnounce
  transactionsAnnouncer net
              

start :: P2p.P2pConfig -> [PeerAddress] -> Db.Db -> IO Network 
start p2pConfig seeds db  = do
    networkState <- NetworkState <$> (P2p.start seeds p2pConfig) <*> pure db
    networkListenerThread <- forkIO $ networkListener networkState
    braggerThread <- forkIO $ bragger networkState
    transactionAnnouncherThread <- forkIO $ transactionsAnnouncer networkState
    return Network{..}

stop :: Network -> IO ()
stop net = do
  killThread $ networkListenerThread net
  killThread $ transactionAnnouncherThread net
  killThread $ braggerThread net
  P2p.stop $ networkP2p $ networkState net

ibroadcast ::  NetworkState -> NetworkMessage -> IO ()
ibroadcast ns msg = P2p.broadcastPayload (networkP2p $ ns) (encodeMessage msg)
           
broadcast :: Network -> NetworkMessage -> IO ()
broadcast net msg = ibroadcast (networkState net) msg

isend :: NetworkState -> PeerAddress -> NetworkMessage -> IO ()
isend net peer msg = P2p.sendPayload  (networkP2p net) peer (encodeMessage msg)
                    
send :: Network -> PeerAddress -> NetworkMessage -> IO ()
send net peer msg = isend (networkState net) peer msg

networkTime :: Network -> IO TimeStamp
networkTime net = P2p.networkTime $ networkP2p $ networkState net


networkDb :: Network -> Db.Db
networkDb net = networkStateDb $ networkState net
