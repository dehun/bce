{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Bce.P2p where

import Bce.Util
import Bce.TimeStamp
import Bce.Logger    
    
import Data.Binary
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Control.Concurrent
import Control.Concurrent.STM    
import Control.Monad
import Control.Monad.Trans
import Control.Applicative   
import Data.Either
import Data.Maybe
import Debug.Trace    
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket.ByteString as SBS    
import qualified Network.Socket as Sock
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut    
import qualified Data.Set as Set
import qualified Data.Map as Map    
import qualified Control.Exception as Exception    


data P2pConfig = P2pConfig {
      p2pConfigBindAddress :: PeerAddress
    , p2pConfigReconnectTimeout :: Int
    , p2pConfigAnnounceTimeout :: Int
    , p2pConfigTimerTimeout :: Int
    , p2pConfigPeersConnectedLimit :: Int
      } deriving (Show, Eq)


data PeerAddress = PeerAddress {
      peerIp :: String
    , peerPort :: Int32
      } deriving (Show, Eq, Generic, Ord)
instance Binary PeerAddress

data P2pState = P2pState {
      p2pConfig :: P2pConfig
    , p2pPeers :: TVar (Set.Set PeerAddress)
    , p2pConnectedPeers :: TVar (Set.Set PeerAddress)
    , p2pConnectingPeers :: TVar (Set.Set PeerAddress)                           
    , p2psRecvChan :: TChan PeerEvent
    , p2pSendChan :: TChan PeerSend
    , p2pPeerTimes :: TVar (Map.Map PeerAddress TimeStamp)
    , p2pPeerThreads :: TVar (Map.Map PeerAddress [ThreadId])
      }

p2pRecvChan p2ps = p2psRecvChan $ p2pState p2ps


data P2p = P2p { p2pState :: P2pState
               , p2pServerListenerThread :: ThreadId
               , p2pReconnectorThread :: ThreadId
               , p2pAnnouncherLoop :: ThreadId} 

data P2pMessage =
                P2pMessageHello PeerAddress
                | P2pMessageAnounce (Set.Set PeerAddress)
                | P2pMessagePayload BS.ByteString
                | P2pMessageTellTime TimeStamp
                deriving (Show, Eq, Generic)
instance Binary P2pMessage

data P2pClientState = P2pClientState {
      clientStateDecoder :: BinGet.Decoder P2pMessage
    , clientStatePeer :: Maybe PeerAddress
      }

clientStateUpdateDecoder :: BinGet.Decoder P2pMessage -> P2pClientState -> P2pClientState
clientStateUpdateDecoder newDecoder p2pState  =
    P2pClientState newDecoder $ clientStatePeer p2pState

clientStateUpdatePeer :: PeerAddress -> P2pClientState -> P2pClientState
clientStateUpdatePeer newPeer p2pState  =
    P2pClientState (clientStateDecoder p2pState) $ Just newPeer


data PeerEvent = PeerDisconnected PeerAddress
                 | PeerMessage PeerAddress BS.ByteString  deriving (Show, Eq)

data PeerSend = PeerSend { sendMsg :: P2pMessage, sendDestination :: Maybe PeerAddress } deriving (Show, Eq)

msgDecoder :: BinGet.Get P2pMessage
msgDecoder = do
  msgLen <- BinGet.getWord64le
  msgBs <- BinGet.getLazyByteString (fromIntegral msgLen) 
  return $ BinGet.runGet Bin.get msgBs :: BinGet.Get P2pMessage

encodeMsg :: P2pMessage -> BS.ByteString
encodeMsg msg =
    let payload = BinPut.runPut $ Bin.put msg
        payloadSize = BinPut.runPut (BinPut.putWord64le $ fromIntegral (BSL.length payload))
    in BSL.toStrict $ mappend payloadSize payload                                          

pollMessageFromClienBuffer :: BS.ByteString ->  State.StateT P2pClientState IO (Maybe (P2pMessage, BS.ByteString))
pollMessageFromClienBuffer bs = do
    decoder <- clientStateDecoder <$> State.get
    olds <- State.get
    let nextDecoder = BinGet.pushChunk decoder bs  :: BinGet.Decoder P2pMessage
    case nextDecoder of
      BinGet.Fail _ _ _ -> error "msg decoding failed"
      BinGet.Partial _ -> do
          State.put (clientStateUpdateDecoder nextDecoder olds)
          return Nothing
      BinGet.Done leftover _ msg -> do
          let newDecoder = BinGet.runGetIncremental msgDecoder
          State.put $ clientStateUpdateDecoder newDecoder olds
          return $ Just (msg, leftover)

pollMessagesFromClientBufer :: BS.ByteString -> State.StateT P2pClientState IO [P2pMessage]
pollMessagesFromClientBufer s' =
    let continue ns msgs = do
          res <- pollMessageFromClienBuffer ns
          case res of
            Just (msg, leftover) ->
                continue leftover (msg:msgs)
            Nothing -> return $ reverse msgs
    in continue s' []

handlePeerMessage :: Sock.Socket -> P2pState -> P2pMessage -> State.StateT P2pClientState IO ()
handlePeerMessage sock p2p msg = do
  liftIO $ logTrace $ "p2p got msg " ++ show msg
  case msg of
    P2pMessageHello peer -> do
                      liftIO $ logTrace $ "hello from " ++ show peer
                      liftIO $ atomically $ do
                              oldPeers <- readTVar (p2pPeers p2p)
                              writeTVar (p2pPeers p2p) (Set.insert peer oldPeers)
                              oldConnectedPeers <- readTVar $ p2pConnectedPeers p2p
                              writeTVar (p2pConnectedPeers p2p) $ Set.insert peer oldConnectedPeers
                      oldState <- State.get
                      State.put $ clientStateUpdatePeer peer oldState
                      sendThread <- liftIO $ forkIO $ clientSendLoop sock peer p2p
                      thisThread <- liftIO $ myThreadId
                      liftIO $ atomically $ do
                              oldThreads <- readTVar (p2pPeerThreads p2p)
                              let newThreads = Map.insert peer [thisThread, sendThread] oldThreads
                              writeTVar (p2pPeerThreads p2p) newThreads
                      return ()
    P2pMessageAnounce peers -> do
             liftIO $ logTrace $ "new peers " ++ show peers
             liftIO $ atomically $ do
                                 oldPeers <- readTVar $ p2pPeers p2p
                                 writeTVar (p2pPeers p2p) $ Set.union peers oldPeers
                                 return ()
    P2pMessagePayload userMsg -> do
             -- TODO: check do we have bloody peer
             peer <- fromJust <$> clientStatePeer <$> State.get
             liftIO $ atomically $ writeTChan (p2psRecvChan p2p)
                                 (PeerMessage peer userMsg)

    P2pMessageTellTime time -> do
      peer <- fromJust <$> clientStatePeer <$> State.get      
      liftIO $ atomically $ do
        oldTimes <- readTVar $ p2pPeerTimes p2p
        let newTimes = Map.insert peer time oldTimes
        writeTVar (p2pPeerTimes p2p) newTimes
        return ()
        

clientRecvLoop :: Sock.Socket -> Sock.SockAddr -> P2pState -> State.StateT P2pClientState IO ()
clientRecvLoop sock addr p2p =
    do
      olds <- State.get      
      s <- liftIO $ SBS.recv sock 1024
      if BS.length s > 0
      then do
          msgs <- pollMessagesFromClientBufer s
          forM_ msgs (\m -> handlePeerMessage sock p2p m)
          clientRecvLoop sock addr p2p
      else do
        liftIO $ logWarning $ "recv loop error"
        peer <- clientStatePeer <$> State.get
        case peer of
          Just peerAddress -> do
              liftIO $ (killPeer peerAddress p2p)
              liftIO $ atomically $ do
                    writeTChan (p2psRecvChan p2p) (PeerDisconnected peerAddress)
          Nothing -> pure ()
        return ()

killPeer :: PeerAddress -> P2pState -> IO Bool
killPeer peer p2p = do
    logWarning $ "killing peer" ++ show peer
    atomically $ do
      oldConnectedPeers <- readTVar $ p2pConnectedPeers p2p
      writeTVar (p2pConnectedPeers p2p) $ Set.delete peer oldConnectedPeers
      oldTimes <- readTVar $ p2pPeerTimes p2p
      writeTVar (p2pPeerTimes p2p) (Map.delete peer oldTimes)  
    peerThreads <- Map.lookup peer <$> (atomically $ readTVar $ p2pPeerThreads p2p)
    case peerThreads of
      Nothing -> return False
      Just threads -> do
                       mapM_ killThread threads
                       return True

clientSendLoop :: Sock.Socket -> PeerAddress -> P2pState -> IO ()
clientSendLoop sock peerAddr p2p = do
  chan <- atomically $ dupTChan $ p2pSendChan p2p
  let loop = do
        snd <- atomically $ readTChan chan
        let encodedMsg = encodeMsg $ sendMsg snd
        case sendDestination snd of
          Nothing -> do
               logDebug $ "broadcast towards " ++ show peerAddr
                           ++ " of size=" ++ show (BS.length encodedMsg)
               SBS.send sock encodedMsg
          Just dst -> if dst == peerAddr
                      then do
                        logDebug $ "sending towards " ++ show peerAddr
                                    ++ " of size=" ++ show (BS.length encodedMsg)
                        SBS.send sock encodedMsg
                      else do
                        logDebug $ "ignoring send towards " ++ show dst
                        return 0
  Exception.catch (forever loop)
               (\e -> do
                  Sock.close sock
                  logDebug $ "killing peer connection at addr" ++ (show peerAddr)
                               ++ "catched" ++ show (e :: Exception.IOException)
                  killPeer peerAddr p2p
                  return ()
                  )
                    
handlePeer :: Sock.Socket -> Sock.SockAddr -> P2pState -> IO ()
handlePeer sock addr p2p = do
  logDebug $ "got peer from addr"  ++ show addr
  recvThread <- forkIO $ do
    State.evalStateT (clientRecvLoop sock addr p2p)
             (P2pClientState (BinGet.runGetIncremental msgDecoder) Nothing)
  return ()

serverMainLoop :: Sock.Socket -> P2pState -> IO ()
serverMainLoop sock p2p =
  Exception.catch 
    (forever $ do
      (conn, addr) <- Sock.accept sock
      connected <- length <$> (atomically $ readTVar $ p2pConnectedPeers p2p)
      if connected <p2pConfigPeersConnectedLimit (p2pConfig p2p) 
      then do SBS.send conn $ encodeMsg $ P2pMessageHello $ p2pConfigBindAddress $ p2pConfig p2p
              handlePeer conn addr p2p
              return ()
      else do
        logDebug "dropping the connection as we already at top of connecte peers"
        Sock.close conn
        return ())
    (\ex -> do
         let e = ex :: Exception.SomeException
         -- TODO: kill all peers
         logWarning "killing all peers"
         toKill <- atomically $ readTVar $ p2pConnectedPeers p2p
         mapM (\p -> killPeer p p2p) $ Set.toList toKill
         Sock.close sock)



startServerListener :: P2pConfig -> P2pState -> IO ThreadId
startServerListener config p2p  = do                       
  sock <- Sock.socket Sock.AF_INET Sock.Stream 0
  Sock.setSocketOption sock Sock.ReuseAddr 1
  Sock.bind sock (Sock.SockAddrInet (fromIntegral $ peerPort $ p2pConfigBindAddress config)
                      (Sock.tupleToHostAddress (read $ peerIp $ p2pConfigBindAddress config)))
  Sock.listen sock 2
  pid <- forkIO $ serverMainLoop sock p2p
  return pid

peerAddressToSockAddr :: PeerAddress -> Sock.SockAddr
peerAddressToSockAddr (PeerAddress host port) =
    Sock.SockAddrInet (fromIntegral port) readedHost
        where readedHost = Sock.tupleToHostAddress $ read host

reconnectPeer :: P2pState -> PeerAddress -> IO ()
reconnectPeer p2p peerAddress = do
      logDebug $ "reconnecting peer" ++ show peerAddress
      proceed <- atomically $ do
                   isConnecting <- Set.member peerAddress <$> readTVar (p2pConnectingPeers p2p)
                   isConnected <- Set.member peerAddress <$> readTVar (p2pConnectedPeers p2p)
                   let r  = or [isConnecting, isConnected]
                   if  r 
                   then return False
                   else do
                     oldConnectingPeers <- readTVar (p2pConnectingPeers p2p)
                     writeTVar (p2pConnectingPeers p2p) (Set.insert peerAddress oldConnectingPeers)
                     return True
      if not proceed
      then do
          logDebug $ "already connecting to " ++ show peerAddress
          return()
      else do
        let removeFromConnecting = do
                                  oldConnecting <- readTVar $ p2pConnectingPeers p2p
                                  writeTVar (p2pConnectingPeers p2p) (Set.delete peerAddress oldConnecting)
        sock <- Sock.socket Sock.AF_INET Sock.Stream 0
        success <- Exception.try $
                         do
                           Sock.connect sock $ peerAddressToSockAddr peerAddress
                           SBS.send sock $ encodeMsg $ P2pMessageHello $ p2pConfigBindAddress $ p2pConfig p2p
                           logDebug $ "connected to " ++ show peerAddress
        case success of
          Right _ -> do

             atomically $ do
                       oldConnected <- readTVar (p2pConnectedPeers p2p)
                       writeTVar (p2pConnectedPeers p2p) (Set.insert peerAddress oldConnected)
                       removeFromConnecting
             forkIO $ handlePeer sock (peerAddressToSockAddr peerAddress) p2p
             return ()
          Left err -> do
             Sock.close sock          
             logDebug $  "connect failed to " ++ show peerAddress
                          ++ "; err: " ++ show (err::Exception.IOException)
             atomically removeFromConnecting

reconnectorLoop :: P2pState -> IO ()
reconnectorLoop p2p = do
    threadDelay (secondsToMicroseconds $ p2pConfigReconnectTimeout $ p2pConfig p2p)
    toConnect <- atomically $ do
                        all <- readTVar $ p2pPeers p2p
                        connected <- readTVar $ p2pConnectedPeers p2p
                        if length connected < p2pConfigPeersConnectedLimit (p2pConfig p2p)
                        then return $ (Set.delete (p2pConfigBindAddress $ p2pConfig p2p)$ (Set.\\) all connected)
                        else return Set.empty
    forM_ toConnect (\p -> forkIO $ reconnectPeer p2p p)
    reconnectorLoop p2p

announcerLoop :: P2pState -> IO ()
announcerLoop p2p = do
  threadDelay (secondsToMicroseconds $ p2pConfigAnnounceTimeout $ p2pConfig p2p)
  (peersToAnnounce, toPeers) <- atomically $ do
                                  (,) <$> (readTVar $ p2pPeers p2p)
                                          <*> (readTVar $ p2pConnectedPeers p2p)
  ibroadcast p2p $ P2pMessageAnounce peersToAnnounce
  announcerLoop p2p

timerLoop :: P2pState -> IO ()
timerLoop p2p = do
  threadDelay (secondsToMicroseconds $ p2pConfigTimerTimeout $ p2pConfig p2p)
  ibroadcast p2p <$> P2pMessageTellTime <$> now
  timerLoop p2p


start :: [PeerAddress] -> P2pConfig -> IO P2p
start seeds config = do
  p2pState <- P2pState config <$> newTVarIO (Set.fromList seeds) <*> newTVarIO Set.empty
         <*> newTVarIO Set.empty <*> newTChanIO <*> newTChanIO
                 <*> newTVarIO Map.empty <*> newTVarIO Map.empty
  p2pServerListenerThread <- startServerListener config p2pState
  p2pReconnectorThread <- forkIO $ reconnectorLoop p2pState
  p2pAnnouncherLoop <- forkIO $ announcerLoop p2pState
  return P2p{..}


stop :: P2p -> IO ()
stop p2p = do
    killThread $ p2pServerListenerThread p2p
    killThread $ p2pReconnectorThread p2p
    killThread $ p2pAnnouncherLoop p2p
    

broadcast :: P2p -> P2pMessage -> IO ()
broadcast p2p msg = ibroadcast (p2pState p2p) msg

send :: P2p -> PeerAddress -> P2pMessage -> IO ()
send p2p peer msg = isend (p2pState p2p) peer msg

ibroadcast :: P2pState -> P2pMessage -> IO ()
ibroadcast p2p msg = do
    atomically $ writeTChan (p2pSendChan p2p) (PeerSend msg Nothing)

isend :: P2pState -> PeerAddress -> P2pMessage -> IO ()
isend p2p peer msg = do
    logDebug $ "p2p queuing send towards peer="  ++ show peer
    atomically $ writeTChan (p2pSendChan p2p) (PeerSend msg $ Just peer)

broadcastPayload :: P2p -> BS.ByteString -> IO ()
broadcastPayload p2p payload = broadcast p2p $ P2pMessagePayload payload

sendPayload :: P2p -> PeerAddress -> BS.ByteString -> IO ()
sendPayload p2p peer payload = send p2p peer $ P2pMessagePayload payload

networkTime :: P2p -> IO TimeStamp
networkTime p2p = do
    times <- atomically $ readTVar $ p2pPeerTimes $ p2pState p2p
    ourTime <- now
    return $ median (ourTime: Map.elems times)
