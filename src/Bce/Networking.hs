{-# LANGUAGE DeriveGeneric #-}

module Bce.Networking where

import qualified Bce.BlockChain as BlockChain
import qualified Bce.Db as Db
import qualified Bce.P2p as P2p
import Bce.Hash

import qualified Data.Binary as Bin
import GHC.Generics (Generic)
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

type PeerAddress = P2p.PeerAddress    

data Network = Network { networkP2p :: P2p.P2p
                       , networkDb :: Db.Db }

data NetworkMessage = Brag Int
                    | Ask Hash
                    | Propose [BlockChain.Block]
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
decodeMessage bs = BinGet.runGet Bin.get (BSL.fromStrict bs)

start :: P2p.P2pConfig -> [P2p.PeerAddress] -> Db.Db -> IO Network 
start p2pConfig seeds db  = do
    network <- Network <$> (P2p.start seeds p2pConfig) <*> pure db
    return network

broadcast :: Network -> NetworkMessage -> IO ()
broadcast net msg = P2p.broadcastPayload (networkP2p net) (encodeMessage msg)

send :: Network -> PeerAddress -> NetworkMessage -> IO ()
send net peer msg = P2p.sendPayload  (networkP2p net) peer (encodeMessage msg)                    
