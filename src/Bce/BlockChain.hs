{-# LANGUAGE DeriveGeneric #-}    

module Bce.BlockChain where

import Bce.Crypto
import Bce.Hash
import Bce.TimeStamp

import Data.Maybe
import Data.Ord    
import qualified Data.Set as Set
import GHC.Int(Int64, Int32)
import GHC.Generics (Generic)


type TransactionId = Hash
type BlockId = Hash        

data TxOutput = TxOutput { outputAmount :: Int64
                         , outputPubKey :: PubKey
                         } deriving (Show, Eq, Generic)

data TxOutputRef = TxOutputRef {
      outputRefTxId :: TransactionId
    , outputRefOutputIdx :: Int32
      } deriving (Show, Eq, Generic)

instance Ord TxOutputRef
    where compare = comparing show

data TxInput = TxInput { inputOutputRef :: TxOutputRef } deriving (Show, Eq, Generic)

data Transaction =
    CoinbaseTransaction {
    txOutputs :: Set.Set TxOutput }
  | Transaction {
      txInputs :: Set.Set TxInput
    , txOutputs :: Set.Set TxOutput
    , txSignature :: Signature } deriving (Show, Eq, Generic)

    
data BlockHeader = BlockHeader {
      bhTransactionsHash :: Hash
    , bhPrevBlockHeaderHash :: Hash
    , bhNonce :: Int64
    , bhWallClockTime :: TimeStamp
    , bhDifficulity :: Int32
    } deriving (Show, Eq, Generic)

data Block = Block { blockHeader:: BlockHeader
                   , blockTransactions :: Set.Set Transaction } deriving (Show, Eq, Generic)

data BlockChain = BlockChain { blockChainBlocks :: [Block] } deriving (Show, Eq, Generic)

                
isCoinbaseTransaction :: Transaction -> Bool    
isCoinbaseTransaction (CoinbaseTransaction _) = True
isCoinbaseTransaction _ = False
