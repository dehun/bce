{-# LANGUAGE DeriveGeneric #-}    

module Bce.BlockChain where

import Data.Maybe
import qualified Data.List as DList
import Bce.Crypto
import Bce.Hash
import GHC.Int(Int64, Int32)
import GHC.Generics (Generic)    


type TimeStamp = Int64

data TxOutput = TxOutput { outputAmount :: Int64
                         , outputPubKey :: PubKey
                         } deriving (Show, Eq, Generic)

data TxOutputRef = TxOutputRef {
      outputRefTxId :: Hash
    , outputRefOutputIdx :: Int32
      } deriving (Show, Eq, Generic)


data TxInput = TxInput { inputOutputRef :: TxOutputRef } deriving (Show, Eq, Generic)

data Transaction =
    CoinbaseTransaction {
    txOutputs :: [TxOutput] }
  | Transaction {
      txInputs :: [TxInput]
    , txOutputs :: [TxOutput]
    , txSignature :: Hash } deriving (Show, Eq, Generic)

data BlockHeader = BlockHeader {
      bhTransactionsHash :: Hash
    , bhPrevBlockHeaderHash :: Hash
    , bhNonce :: Int64
    , bhWallClockTime :: TimeStamp
    , bhDifficulity :: Int32
    } deriving (Show, Eq, Generic)

data Block = Block { blockHeader:: BlockHeader
                   , blockTransactions :: [Transaction] } deriving (Show, Eq, Generic)

data BlockChain = BlockChain { blockChainBlocks :: [Block] } deriving (Show, Eq, Generic)

