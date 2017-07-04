{-# LANGUAGE DeriveGeneric #-}

module Bce.Networking where

import qualified Bce.BlockChain as BlockChain
import qualified Bce.DbFs as Db
import qualified Bce.VerifiedDb as VerifiedDb
import qualified Bce.P2p as P2p
import Bce.Logger    
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


type PeerAddress = P2p.PeerAddress    

data Network = Network { networkP2p :: P2p.P2p
                       , networkDb :: Db.Db }

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

handlePeerMessage :: Network -> PeerAddress -> NetworkMessage -> StateT NetworkListenerState IO ()
handlePeerMessage net peer msg = do
    let db = networkDb net
    liftIO $ logDebug $ "got message " ++ show msg
    case msg of
      Brag braggedLen -> do
               (dbLen, topBlock) <- liftIO $ Db.getLongestHead db
               if dbLen < braggedLen
               then do
                 liftIO $ logInfo $ "saw bragger with length!" ++ show braggedLen
                 s <- get
                 if Map.member peer (networkListenerActiveSyncs s)
                 then liftIO $ logDebug "already syncing from that bragger"
                 else do
                   liftIO $ logInfo $  "starting sync from " ++ show peer
                   let askFrom = hash topBlock
                   let newSync = NetworkBlocksSyncState 1 askFrom
                   let ns = s{networkListenerActiveSyncs=Map.insert peer newSync $ networkListenerActiveSyncs s}
                   put ns
                   liftIO $ send net peer $ Ask askFrom
               else return ()
      Ask fromHash -> do
               blocksOpt <- liftIO $ Db.getBlocksFrom (networkDb net) fromHash
               case blocksOpt of
                 Just blocks -> do
                     liftIO $ logInfo $ "proposing blocks from " ++ show fromHash
                     liftIO $ send net peer $ Propose $ take maxBlocksSyncBatch $ blocks
                 Nothing -> do
                     liftIO $ logInfo $ "dunno from hash" ++ show fromHash
                     liftIO $ send net peer $ Dunno
      Propose blocks -> do
               modify (\oldState -> oldState{networkListenerActiveSyncs=
                                                 Map.delete peer $ networkListenerActiveSyncs oldState})
               liftIO $ logDebug $ "pushing blocks to chain" ++ show (length blocks)
               liftIO $ VerifiedDb.verifyAndPushBlocks (networkDb net) blocks
      Dunno -> do
               oldState <- get
               let oldSyncStateOpt = Map.lookup peer $ networkListenerActiveSyncs oldState
               case oldSyncStateOpt of
                 Nothing -> liftIO $ logWarning $ "got dunno for unstarted sync from "  ++ show peer
                 Just oldSyncState -> do
                     let askSkipInterval = 2 ^ (networkBlocksSyncAccelerationKoef oldSyncState)
                     let oldFromHash = networkBlockSyncLastAsk oldSyncState
                     oldBlocks <- liftIO $ Db.getBlocksTo db oldFromHash askSkipInterval
                     let newFromHash = case oldBlocks of
                                        [] -> traceShowId $ hash initialBlock
                                        _ -> hash $ last oldBlocks
                     let newSyncState = oldSyncState{ networkBlocksSyncAccelerationKoef =
                                                         1 + networkBlocksSyncAccelerationKoef oldSyncState
                                                    , networkBlockSyncLastAsk = newFromHash }
                     put oldState{networkListenerActiveSyncs=Map.insert peer newSyncState
                                                              $ networkListenerActiveSyncs oldState}
                     liftIO $ logInfo $ "got dunno; asking from" ++ show newFromHash
                                     ++ " with skip " ++ show askSkipInterval
                     liftIO $ send net peer $ Ask newFromHash
      PushTransactions transactions -> do
        liftIO $ VerifiedDb.verifyAndPushTransactions (networkDb net) transactions
        return ()


data NetworkBlocksSyncState = NetworkBlocksSyncState { networkBlocksSyncAccelerationKoef :: Int
                                                     , networkBlockSyncLastAsk :: Hash } deriving (Show)
data NetworkListenerState = NetworkListenerState {
      networkListenerActiveSyncs :: Map.Map PeerAddress NetworkBlocksSyncState } deriving (Show)


networkListener :: Network -> IO ()
networkListener net = do
      chan <- atomically $ dupTChan $ P2p.p2pRecvChan $ networkP2p net
      let initialState = NetworkListenerState Map.empty
      let loop = do
            msg <- liftIO $ atomically $ readTChan chan
            case msg of
              P2p.PeerDisconnected peer ->
                  modify (\oldState -> oldState{networkListenerActiveSyncs=
                                                    Map.delete peer $ networkListenerActiveSyncs oldState})
              P2p.PeerMessage peer bs -> handlePeerMessage net peer $ decodeMessage bs
            loop 
      evalStateT loop initialState

bragger :: Network -> IO ()
bragger net =
    let loop = do
          threadDelay (secondsToMicroseconds $ 5)
          (length, _) <- Db.getLongestHead (networkDb net)
          logTrace $ "bragging with length of " ++ show length
          broadcast net $ Brag length
          loop
    in loop

transactionsAnnouncer :: Network -> IO ()
transactionsAnnouncer net = do
  threadDelay (secondsToMicroseconds $ 5)
  transactionsToAnnounce <- Db.getTransactions $ networkDb net
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
