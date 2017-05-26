module BlockChain where

import Data.Maybe
import qualified Data.List as DList

type Hash = String
type PubKey = String    


type TimeStamp = Int
type WalletAddress = String    

data TxOutput = TxOutput { outputAmount :: Double
                         , outputPubKey :: PubKey
                         } deriving (Show, Eq)
data TxOutputRef = TxOutputRef {
      outputRefTxId :: Hash
    , outputRefOutputIdx :: Int
      } deriving (Show, Eq)


data TxInput = TxInput { inputOutputRef :: TxOutputRef
                       , inputSignature :: Hash } deriving (Show, Eq)

data Transaction =
    CoinbaseTransaction {
      txOutputs :: [TxOutput] }
  | Transaction {
        txInputs :: [TxInput]
      , txOutputs :: [TxOutput] } deriving (Show, Eq)

data BlockHeader = BlockHeader {
      bhTransactionsHash :: Hash
    , bhPrevBlockHeaderHash :: Hash
    , bhNonce :: Int
    , bhWallClockTime :: TimeStamp
    } deriving (Show, Eq)

data Block = Block { blockHeader:: BlockHeader
                   , blockTransactions :: [Transaction] } deriving (Show, Eq)

data BlockChain = BlockChain { blockChainBlocks :: [Block] } deriving (Show, Eq)

-- ^^^ data structures goes up here ^^^
-- vvv experimental shit goes here  vvv

elemAt :: [a] -> Int -> Maybe a
elemAt xs idx = continue 0 xs idx
    where continue cidx (x:xs) idx
              | cidx == idx = Just x
              | otherwise = continue (cidx + 1) xs idx
          continue _ [] _ = Nothing

                

resolveTransaction :: Block -> Int -> Maybe Transaction
resolveTransaction b idx = elemAt (blockTransactions b) idx

resolveInput :: Transaction -> Int -> Maybe TxInput
resolveInput (CoinbaseTransaction _) _ = Nothing
resolveInput (Transaction ins outs) idx = elemAt ins idx

resolveOutput :: Transaction -> Int -> Maybe TxOutput
resolveOutput tx idx = elemAt (txOutputs tx) idx

-- resolveOutputInChain :: BlockChain -> TxOutputRef -> Maybe TxOutput
-- resolveOutputInChain bc (TxOutputRef blockIdx txIdx outputIdx) = do
--       block <- resolveBlock bc blockIdx
--       tx <- resolveTransaction block txIdx
--       resolveOutput tx outputIdx

resolveBlock :: BlockChain -> Int -> Maybe Block                
resolveBlock bc blockIndex = elemAt (blockChainBlocks bc) blockIndex

verifyTransaction :: BlockChain -> Transaction -> Bool
verifyTransaction bc tx =
    let resolveInput i =
            resolveBlock
            where ref = inputOutputRef i
        resolvedInputs = map resolveInput $ txInputs tx
    in undefined
                

verifyBlock :: Block -> Bool
verifyBlock = undefined

verifyBlockChain :: BlockChain -> Bool
verifyBlockChain = undefined
