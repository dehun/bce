{-# LANGUAGE DeriveGeneric #-}

module Bce.RestTypes where

import Bce.BlockChain
import Bce.Hash
import Bce.BlockChainSerialization
import Bce.BlockChainHash
import GHC.Generics    

import qualified Data.Set as Set    
import Data.Aeson hiding (json)    


data WalletBalance = WalletBalance { outputs :: Set.Set TxOutputRef
                                   , unconfirmed :: Set.Set TxOutputRef } deriving (Show, Eq, Generic)
instance ToJSON WalletBalance
instance FromJSON WalletBalance    
    
data RestTransaction = RestTransaction {
      tx :: Transaction
    , txId :: TransactionId
      } deriving (Generic, Eq, Show)

instance ToJSON RestTransaction
instance FromJSON RestTransaction                         
    
data RestBlock = RestBlock {
      blockHeader :: BlockHeader
    , transactions :: [RestTransaction]
      } deriving (Generic, Eq, Show)

instance ToJSON RestBlock
instance FromJSON RestBlock    

blockToRestBlock :: Block -> RestBlock
blockToRestBlock blk =
    let hdr = Bce.BlockChain.blockHeader blk
        txs = map (\tx -> RestTransaction tx (transactionId blk tx)) $ Set.toList (blockTransactions blk)
    in RestBlock hdr txs

data Head = Head {
      headLength :: Int
    , headBlockId :: BlockId
      } deriving (Generic, Eq, Show)

instance ToJSON Head
instance FromJSON Head
