module Bce.Miner where

import Bce.Crypto
import Bce.Hash
import Bce.BlockChain
import Bce.BlockChainHash    
import Bce.InitialBlock
import Bce.Difficulity
import Bce.TimeStamp
import Bce.Logger
import qualified Bce.DbFs as Db
import qualified Bce.VerifiedDb as VerifiedDb    

import Control.Monad    
import Data.Either    
import Debug.Trace
import GHC.Int (Int64)
import System.Random
import Control.Exception    

import qualified Data.Set as Set    
import Control.Monad
import Control.Concurrent    
import Control.Concurrent.STM
import qualified Data.ByteString as BS


tryGenerateBlock :: TimeStamp -> Int64 -> BlockId -> Set.Set Transaction -> Difficulity -> Maybe Block
tryGenerateBlock time rnd prevBlockId txs target = do
  let header = BlockHeader
                 (hash txs)
                 prevBlockId
                 rnd
                 (fromIntegral time)
                 (fromIntegral target)
  let candidate = Block header txs
  if blockDifficulity candidate >= target
  then Just candidate
  else Nothing
    

coinbaseTransaction :: Db.Db -> PubKey -> Set.Set Transaction -> IO Transaction
coinbaseTransaction db ownerKey txs = do
    Just maxReward <- Db.maxCoinbaseReward db (Set.toList txs)
    return $ CoinbaseTransaction $ Set.fromList [TxOutput maxReward ownerKey]


findBlock :: Db.Db -> PubKey -> Timer -> IO Block
findBlock db ownerKey timer = do
    time <- timer
    rnd <- randomIO :: IO Int64
    rtxs <- Db.getTransactions db              
    cbtx <- coinbaseTransaction db ownerKey rtxs
    txs <- Set.insert cbtx <$> Db.getTransactions db
    (_, topBlock) <- Db.getLongestHead db
    next <- tryGenerateBlock time rnd <$> pure (blockId topBlock) <*> pure txs  <*> Db.getNextDifficulity db
    case next of
        Just blk -> return  blk
        Nothing -> findBlock db ownerKey timer

growOneBlock :: Db.Db -> PubKey -> Timer -> IO ()
growOneBlock db ownerKey timer = do
     nextBlock <- findBlock db ownerKey timer
     v <- VerifiedDb.verifyAndPushBlock db nextBlock
     (headLength, topBlock) <- Db.getLongestHead db
     nextDiff <- Db.getNextDifficulity db
     logDebug $ "got chain of length " ++ show headLength
                  ++ "; block difficulity is " ++ (show $ blockDifficulity topBlock)
                  ++ "; next difficulity is " ++ (show nextDiff)
                  ++ "; blockhash is " ++ (show $ hash topBlock)
     assert v (return ())

mineForever :: Db.Db -> PubKey -> Timer -> IO ()
mineForever db pubkey timer = forever $ growOneBlock db pubkey timer
