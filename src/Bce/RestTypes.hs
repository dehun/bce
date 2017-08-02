{-# LANGUAGE DeriveGeneric     #-}

module Bce.RestTypes where

import Bce.BlockChain
import Bce.Hash
import Bce.BlockChainSerialization
import Bce.BlockChainHash
import GHC.Generics    

import qualified Data.Set as Set    
import Data.Aeson hiding (json)    


data WalletBalance = WalletBalance { outputs :: Set.Set TxOutputRef } deriving (Show, Eq, Generic)
instance ToJSON WalletBalance                   
    
data RestTransaction = RestTransaction {
      tx :: Transaction
    , txId :: TransactionId
      } deriving (Generic, Eq, Show)

instance ToJSON RestTransaction                     
    
data RestBlock = RestBlock {
      blockHeader :: BlockHeader
    , transactions :: [RestTransaction]
      } deriving (Generic, Eq, Show)

instance ToJSON RestBlock

blockToRestBlock :: Block -> RestBlock
blockToRestBlock blk =
    let hdr = Bce.BlockChain.blockHeader blk
        txs = map (\tx -> RestTransaction tx (transactionId blk tx)) $ Set.toList (blockTransactions blk)
    in RestBlock hdr txs
