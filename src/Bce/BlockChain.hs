module Bce.BlockChain where

import Data.Maybe
import qualified Data.List as DList
import Bce.Crypto
import Bce.Hash    

type TimeStamp = Int

data TxOutput = TxOutput { outputAmount :: Double
                         , outputPubKey :: PubKey
                         } deriving (Show, Eq)
data TxOutputRef = TxOutputRef {
      outputRefTxId :: Hash
    , outputRefOutputIdx :: Int
      } deriving (Show, Eq)


data TxInput = TxInput { inputOutputRef :: TxOutputRef } deriving (Show, Eq)

data Transaction =
    CoinbaseTransaction {
      txOutputs :: [TxOutput] }
  | Transaction {
        txInputs :: [TxInput]
      , txOutputs :: [TxOutput]
      , txSignature :: Hash } deriving (Show, Eq)

data BlockHeader = BlockHeader {
      bhTransactionsHash :: Hash
    , bhPrevBlockHeaderHash :: Hash
    , bhNonce :: Int
    , bhWallClockTime :: TimeStamp
    } deriving (Show, Eq)

data Block = Block { blockHeader:: BlockHeader
                   , blockTransactions :: [Transaction] } deriving (Show, Eq)

data BlockChain = BlockChain { blockChainBlocks :: [Block] } deriving (Show, Eq)
