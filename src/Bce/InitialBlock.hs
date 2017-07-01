module Bce.InitialBlock where
import Bce.BlockChain
import Bce.Hash
import Bce.Crypto        
import Bce.BlockChainHash
import Bce.Difficulity

import GHC.Int (Int64)
import Data.List
import qualified Data.Set as Set    
import Data.Maybe
import qualified Data.ByteString as BS

initialBlockGenerator nonce =
    Block (BlockHeader (hash initialTransactions)
                      (hash (0 :: Int64)) nonce 1496243949
                      (fromIntegral defaultDifficulity)) initialTransactions
    where initialTransactions = Set.fromList [CoinbaseTransaction (Set.fromList [TxOutput 50 $ PubKey BS.empty])]

initialBlock = fromJust 
               $ find (\b -> defaultDifficulity == blockDifficulity b)
               $ map initialBlockGenerator [1..]

