module Bce.InitialBlock where
import Bce.BlockChain
import Bce.Hash
import Bce.Crypto        
import Bce.BlockChainHash
import Bce.Difficulity
import Bce.Verified    

import GHC.Int (Int64)
import System.IO.Unsafe
import Data.IORef    
import Data.List
import qualified Data.Set as Set    
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as C8

initialBlockKeyPair = KeyPair (PubKey $ fst $ BS16.decode $ C8.pack "074b20241bd89c01fd55cfb6414640976d96c85f7d756d52f73b212a390e1e09")
                              (PrivKey $ fst $ BS16.decode $ C8.pack "7384eacce80da2eea65d7744e452c826f219039a135cca717a60c519a041f88d")

initialBlockGenerator nonce =
    Block (BlockHeader (hash initialTransactions)
                      (hash (0 :: Int64)) nonce 1496243949
                      (fromIntegral defaultDifficulity)) initialTransactions
    where initialTransactions = Set.fromList [CoinbaseTransaction (Set.fromList [TxOutput 50 $ keyPairPub initialBlockKeyPair])]

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


verifiedInitialBlock = VerifiedBlock initialBlock
