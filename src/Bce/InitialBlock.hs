module Bce.InitialBlock where
import Bce.BlockChain
import Bce.Hash
import Bce.BlockChainHash
import Bce.Difficulity    

import GHC.Int (Int64)
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS

initialBlockGenerator nonce =
    Block (BlockHeader (hash initialTransactions)
                      (hash (0 :: Int64)) nonce 1496243949
                      (fromIntegral defaultDifficulity)) initialTransactions
    where initialTransactions = [CoinbaseTransaction [TxOutput 0 BS.empty]]

initialBlock = fromJust 
               $ find (\b -> defaultDifficulity == blockDifficulity b)
               $ map initialBlockGenerator [1..]

