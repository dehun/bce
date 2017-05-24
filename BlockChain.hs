import Data.Maybe
import qualified Data.List as DList

elemAt :: [a] -> Int -> Maybe a
elemAt xs idx = continue 0 xs idx
    where continue cidx (x:xs) idx
              | cidx == idx = Just x
              | otherwise = continue (cidx + 1) xs idx
          continue _ [] _ = Nothing

type Hash = String
type PubKey = String    


type TimeStamp = Int
type WalletAddress = String    


data BlockHeader = BlockHeader {
      bhTransactionsHash :: Hash
    , bhPrevBlockHeaderHash :: Hash
    , bhNonce :: Int
    , bhWallClockTime :: TimeStamp
    } deriving (Show, Eq)



data TxOutput = TxOutput { outputAmount :: Double
                         , outputDestination :: WalletAddress
                         , outputPubKey :: PubKey
                         } deriving (Show, Eq)


data TxInput = TxInput { inputBlockIndex :: Int
                       , inputTxIndex :: Int
                       , inputOutputIndex :: Int
                       , signature :: Hash } deriving (Show, Eq)

data Transaction =
    CoinbaseTransaction {
      outputs :: [TxOutput] }
  | Transaction {
        inputs :: [TxInput]
      , outputs :: [TxOutput] } deriving (Show, Eq)

resolveTransaction :: Block -> Int -> Maybe Transaction
resolveTransaction b idx = elemAt (blockTransactions b) idx


verifyTransaction :: BlockChain -> Transaction -> Bool
verifyTransaction bc tx =
    let resolveInput i = undefined
    in undefined

data Block = Block { blockHeader:: BlockHeader
                   , blockTransactions :: [Transaction] } deriving (Show, Eq)

data BlockChain = BlockChain { blockChainBlocks :: [Block] } deriving (Show, Eq)

resolveBlock :: BlockChain -> Int -> Maybe Block                
resolveBlock bc blockIndex = elemAt (blockChainBlocks bc) blockIndex

verifyBlockChain :: BlockChain -> Bool
verifyBlockChain = undefined

verifyBlock :: Block -> Bool
verifyBlock = undefined
