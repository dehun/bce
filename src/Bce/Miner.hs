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

import qualified Data.Set as Set    
import Control.Monad
import Control.Concurrent    
import Control.Concurrent.STM
import qualified Data.ByteString as BS


tryGenerateBlock :: TimeStamp -> Int64 -> Block -> Set.Set Transaction -> Difficulity -> Maybe Block
tryGenerateBlock time rnd prevBlock txs target = do
  let header = BlockHeader
                 (hash txs)
                 (hash $ blockHeader prevBlock)
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

-- TODO: split mining into finding block and pushing it to db
growChain :: Db.Db -> PubKey -> Timer -> IO Bool
growChain db ownerKey timer = do
    time <- timer
    rnd <- randomIO :: IO Int64
    generated <- do
      rtxs <- Db.getTransactions db              
      cbtx <- coinbaseTransaction db ownerKey rtxs
      txs <- Set.insert cbtx <$> Db.getTransactions db
      (_, topBlock) <- Db.getLongestHead db
      next <- tryGenerateBlock time rnd <$> pure topBlock <*> pure txs  <*> Db.getNextDifficulity db
      case next of
        Just blk -> VerifiedDb.verifyAndPushBlock db blk
        Nothing -> return False
    if generated
    then do
      (headLength, topBlock) <- Db.getLongestHead db
      nextDiff <- Db.getNextDifficulity db
      logDebug $ show time ++ " got chain of length " ++ show headLength
                  ++ "; block difficulity is " ++ (show $ blockDifficulity topBlock)
                  ++ "; next difficulity is " ++ (show nextDiff)
                  ++ "; blockhash is " ++ (show $ hash topBlock)
      return True
    else return False

growOneBlock :: Db.Db -> PubKey -> Timer -> IO ()
growOneBlock db ownerKey timer = do
     r <- growChain db ownerKey timer
     if r then return ()
     else growOneBlock db ownerKey timer


mineForever :: Db.Db -> PubKey -> Timer -> IO ()
mineForever db pubkey timer = forever $ growChain db pubkey timer
