module Bce.InitialBlock where
import Bce.BlockChain
import Bce.Hash
import Bce.BlockChainHash
import GHC.Int (Int64)    

initialBlock = Block (BlockHeader (hash initialTransactions) (hash (0 :: Int64)) 0 1496243949) initialTransactions
               where initialTransactions = [CoinbaseTransaction [TxOutput 0 ""]]
