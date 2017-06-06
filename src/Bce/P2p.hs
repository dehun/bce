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


data P2pConfig = P2pConfig {
      p2pConfigBindAddress :: PeerAddress
    , p2pPeersMemoryLimit :: Int32
      } deriving (Show)


data PeerAddress = PeerAddress {
      peerIp :: String
    , peerPort :: Int32
      } deriving (Show, Eq, Generic)
instance Binary PeerAddress                                          


data P2p = P2p {
      p2pConfig :: P2pConfig
    , p2pPeers :: TVar [PeerAddress]
    , p2pRecvChan :: TChan PeerEvent
    , p2pSendChan :: TChan BS.ByteString
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

clientRecvLoop :: Sock.Socket -> Sock.SockAddr -> TChan PeerEvent -> State.StateT P2pClientState IO ()
clientRecvLoop sock addr chan =
    do
      olds <- State.get      
      s <- liftIO $ SBS.recv sock 1024
      if BS.length s > 0
      then do
          dispatchMessagesFromClientBuffer s addr chan
          clientRecvLoop sock addr chan
      else do
        error "client disconnected"
        return ()

clientSendLoop :: Sock.Socket -> Sock.SockAddr -> IO ()
clientSendLoop sock addr =
    return ()
                    
handleClient :: Sock.Socket -> Sock.SockAddr -> TChan PeerEvent -> TChan PeerSend -> IO ()
handleClient sock addr recvChan sendChan = do
  forkIO (State.evalStateT (clientRecvLoop sock addr recvChan)
                   (P2pClientState $  BinGet.runGetIncremental msgDecoder))
  forkIO $ clientSendLoop sock addr
  return ()

serverMainLoop :: Sock.Socket -> TChan PeerEvent -> TChan PeerSend -> IO ()
serverMainLoop sock  recvChan sendChan =
    forever $ do
      (conn, addr) <- Sock.accept sock
      handleClient conn addr recvChan sendChan
      return ()

startServerListener :: P2pConfig -> TChan PeerEvent -> TChan PeerSend -> IO ()
startServerListener config recvChan sendChan  = do                       
  sock <- Sock.socket Sock.AF_INET Sock.Stream 0
  Sock.bind sock (Sock.SockAddrInet (fromIntegral $ peerPort $ p2pConfigBindAddress config) Sock.iNADDR_ANY)
  Sock.listen sock 2
  forkIO $ serverMainLoop sock recvChan sendChan
  return ()
  
start :: [PeerAddress] -> P2pConfig -> IO P2p
start seeds config = do
  p2p <- P2p config <$> newTVarIO seeds <*> newTChanIO <*> newTChanIO
  startServerListener config (p2pRecvChan p2p) (p2pSendChan p2p)
  return p2p

  
    
    
    
