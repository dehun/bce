{-# LANGUAGE FlexibleInstances #-}
module Bce.BlockChainHash where

import Bce.Hash
import Bce.BlockChain

import qualified Crypto.Hash.SHA256 as Sha
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL    
import qualified Data.ByteString.Builder as BSB    
import qualified Data.ByteString.Base16 as B16
import GHC.Int(Int64)    

instance Hashable Block where
    hash (Block header txs) = hash header

int64ToBs :: Int64 -> BS.ByteString
int64ToBs x = BSL.toStrict $ BSB.toLazyByteString $ BSB.int64BE x

instance Hashable BlockHeader where
    hash (BlockHeader txHash prevBlockHash nonce wallclock) =
        hash (mconcat [hashBs txHash, hashBs prevBlockHash, int64ToBs nonce, int64ToBs wallclock])

instance Hashable Transaction where
    hash (CoinbaseTransaction outputs) = hash $ mconcat $ map (\o -> hashBs $ hash  o) outputs
    hash (Transaction inputs outputs sig) = hash $ mconcat [ mconcat $ map (\i -> hashBs $ hash i) inputs
                                                      , mconcat $ map (\o -> hashBs $ hash o) outputs
                                                      , hashBs sig ]
                                              
instance Hashable TxOutput where
    hash (TxOutput amount pubkey) = hash $ mconcat [hashBs $ hash amount, hashBs $ hash pubkey ] 

instance Hashable TxInput where
    hash input = undefined --  TODO: implement ,me

instance Hashable [Transaction] where
    hash txs = hash $ mconcat $ map (hashBs . hash) txs
