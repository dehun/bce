import Bce.Crypto
import Bce.Hash
import Bce.BlockChain
import Bce.BlockChainHash    
import Bce.BlockChainVerification

import GHC.Int (Int64)

import Data.Time.Clock
import Data.Time.Clock.POSIX

initialBlock = Block (BlockHeader (hash initialTransactions) (hash (0 :: Int64)) 0 1496213661) initialTransactions
               where initialTransactions = [CoinbaseTransaction [TxOutput 0 ""]]              

main = do
  putStrLn "hi miner"
