{-# LANGUAGE DeriveGeneric #-}

module Bce.P2p where

import Data.Binary
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Control.Concurrent
import Control.Concurrent.STM    
import Control.Monad
import Control.Monad.Trans
import Data.Either    
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket.ByteString as SBS    
import qualified Network.Socket as Sock
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Set as Set    


data P2pConfig = P2pConfig {
      p2pConfigBindAddress :: PeerAddress
    , p2pConfigConnectedPeersLimit :: Int
    , p2pConfigPeersLimit :: Int
    , p2pConfigReconnectTimeout :: Int
      } deriving (Show)


data PeerAddress = PeerAddress {
      peerIp :: String
    , peerPort :: Int32
      } deriving (Show, Eq, Generic, Ord)
instance Binary PeerAddress                                          


data P2p = P2p {
      p2pConfig :: P2pConfig
    , p2pPeers :: TVar (Set.Set PeerAddress)
    , p2pConnectedPeers :: TVar (Set.Set PeerAddress)
    , p2pConnectingPeers :: TVar (Set.Set PeerAddress)                           
    , p2pRecvChan :: TChan PeerEvent
    , p2pSendChan :: TChan PeerSend
      } 


data P2pMessage =
                P2pMessageHello { p2pm_hello_sender :: PeerAddress }
                deriving (Show, Eq, Generic)
instance Binary P2pMessage

data P2pClientState = P2pClientState {
      clientStateDecoder :: BinGet.Decoder P2pMessage
      } 


data PeerEvent = PeerDisconnected { disconnectedPeer ::PeerAddress }
                 | PeerMessage { dataPeer :: PeerAddress, peerMessage :: P2pMessage } deriving (Show, Eq)

data PeerSend = PeerSend { sendMsg :: P2pMessage, sendDestination :: Maybe PeerAddress } deriving (Show, Eq)

sockAddrToPeerAddr :: Sock.SockAddr -> PeerAddress
sockAddrToPeerAddr = undefined

peerAddressToSockAddr :: PeerAddress -> Sock.SockAddr
peerAddressToSockAddr = undefined

msgDecoder :: BinGet.Get P2pMessage
msgDecoder = do
  msgLen <- BinGet.getInt64le
  msgBs <- BinGet.getLazyByteString (fromIntegral msgLen) -- TODO: handle 
  return $ BinGet.runGet Bin.get msgBs :: BinGet.Get P2pMessage  

dispatchMessagesFromClientBuffer :: BS.ByteString -> Sock.SockAddr -> TChan PeerEvent -> State.StateT P2pClientState IO ()
dispatchMessagesFromClientBuffer bs addr outputChan = do
    decoder <- clientStateDecoder <$> State.get
    let nextDecoder = BinGet.pushChunk decoder bs  :: BinGet.Decoder P2pMessage
    case nextDecoder of
      BinGet.Fail _ _ _ -> error "msg decoding failed"
      BinGet.Partial _ -> do
          State.put (P2pClientState nextDecoder)
          return ()
      BinGet.Done leftover _ msg -> do
          liftIO $ atomically $ writeTChan outputChan (PeerMessage (sockAddrToPeerAddr addr) msg)
          let leftoverDecoder = BinGet.runGetIncremental msgDecoder
          State.put $ P2pClientState (BinGet.pushChunk leftoverDecoder leftover)

clientRecvLoop :: Sock.Socket -> Sock.SockAddr -> P2p -> State.StateT P2pClientState IO ()
clientRecvLoop sock addr p2p =
    do
      olds <- State.get      
      s <- liftIO $ SBS.recv sock 1024
      if BS.length s > 0
      then do
          dispatchMessagesFromClientBuffer s addr (p2pRecvChan p2p)
          clientRecvLoop sock addr p2p
      else do
        error "client disconnected"
        return ()

clientSendLoop :: Sock.Socket -> Sock.SockAddr -> IO ()
clientSendLoop sock addr =
    return ()
                    
handlePeer :: Sock.Socket -> Sock.SockAddr -> P2p -> IO ()
handlePeer sock addr p2p = do
  forkIO (State.evalStateT (clientRecvLoop sock addr p2p)
                   (P2pClientState $ BinGet.runGetIncremental msgDecoder))
  forkIO $ clientSendLoop sock addr
  return ()

serverMainLoop :: Sock.Socket -> P2p -> IO ()
serverMainLoop sock p2p =
    forever $ do
      (conn, addr) <- Sock.accept sock
      handlePeer conn addr p2p
      return ()

startServerListener :: P2pConfig -> P2p -> IO ()
startServerListener config p2p  = do                       
  sock <- Sock.socket Sock.AF_INET Sock.Stream 0
  Sock.bind sock (Sock.SockAddrInet (fromIntegral $ peerPort $ p2pConfigBindAddress config) Sock.iNADDR_ANY)
  Sock.listen sock 2
  forkIO $ serverMainLoop sock p2p
  return ()

secondsToMicroseconds :: Int -> Int
secondsToMicroseconds x = x * 1000000

reconnectPeer :: P2p -> PeerAddress -> IO ()
reconnectPeer p2p peerAddress = do
      proceed <- atomically $ do
                   isConnecting <- Set.member peerAddress <$> readTVar (p2pConnectingPeers p2p)
                   isConnected <- Set.member peerAddress <$> readTVar (p2pConnectedPeers p2p)
                   let r  = or [isConnecting, isConnected]
                   if  r 
                   then return False
                   else do
                     writeTVar (p2pConnectingPeers p2p) <$>
                                   (Set.insert peerAddress <$> readTVar (p2pConnectingPeers p2p))
                     return True
      if not proceed
      then return()
      else do
        sock <- Sock.socket Sock.AF_INET Sock.Stream 0
        Sock.connect sock $ peerAddressToSockAddr peerAddress
        let removeFromConnecting = do
                                  oldConnecting <- readTVar $ p2pConnectingPeers p2p
                                  writeTVar (p2pConnectingPeers p2p) (Set.delete peerAddress oldConnecting)
        success <- Sock.isConnected sock
        if success 
        then do
          atomically $ do
               oldConnected <- readTVar (p2pConnectedPeers p2p)
               writeTVar (p2pConnectedPeers p2p) (Set.insert peerAddress oldConnected)
               removeFromConnecting
          forkIO $ handlePeer sock (peerAddressToSockAddr peerAddress) p2p
          return ()
        else atomically removeFromConnecting
      
      

reconnectorLoop :: P2p -> IO ()
reconnectorLoop p2p = do
    threadDelay (p2pConfigReconnectTimeout $ p2pConfig p2p)
    toConnect <- atomically $ do
                        all <- readTVar $ p2pPeers p2p
                        connected <- readTVar $ p2pConnectedPeers p2p
                        return $ (Set.\\) all connected
    forM_ toConnect (\p -> forkIO $ reconnectPeer p2p p)
    reconnectorLoop p2p
  
start :: [PeerAddress] -> P2pConfig -> IO P2p
start seeds config = do
  p2p <- P2p config <$> newTVarIO (Set.fromList seeds) <*> newTVarIO Set.empty
         <*> newTVarIO Set.empty <*> newTChanIO <*> newTChanIO
  startServerListener config p2p
  forkIO $ reconnectorLoop p2p 
  return p2p

  
    
    
    
