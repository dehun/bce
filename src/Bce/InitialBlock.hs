module Bce.InitialBlock where
import Bce.BlockChain
import Bce.Hash
import Bce.Crypto        
import Bce.BlockChainHash
import Bce.Difficulity

import GHC.Int (Int64)
import GHC.IO.Unsafe
import Data.IORef    
import Data.List
import qualified Data.Set as Set    
import Data.Maybe
import qualified Data.ByteString as BS

initialBlockGenerator nonce =
    Block (BlockHeader (hash initialTransactions)
                      (hash (0 :: Int64)) nonce 1496243949
                      (fromIntegral defaultDifficulity)) initialTransactions
    where initialTransactions = Set.fromList [CoinbaseTransaction (Set.fromList [TxOutput 50 $ PubKey BS.empty])]

initialBlock_ = fromJust 
               $ find (\b -> defaultDifficulity == blockDifficulity b)
               $ map initialBlockGenerator [1..]


initialBlockCache :: IORef (Maybe Block)
initialBlockCache = unsafePerformIO $ newIORef Nothing

initialBlock :: Block                    
initialBlock = unsafePerformIO $ do
                 cache <- readIORef initialBlockCache
                 case cache of
                   Just val -> return val
                   Nothing -> do
                              writeIORef initialBlockCache $ Just initialBlock_
                              return initialBlock_

