module Bce.Miner where

import Bce.Crypto
import Bce.Hash
import Bce.BlockChain
import Bce.BlockChainHash    
import Bce.BlockChainVerification
import Bce.InitialBlock
import Bce.Difficulity
import Bce.TimeStamp
import Bce.Logger
import qualified Bce.DbFs as Db

import Data.Either    
import Debug.Trace
import GHC.Int (Int64)
import System.Random    

import Control.Monad
import Control.Concurrent    
import Control.Concurrent.STM    


tryGenerateBlock :: TimeStamp -> Int64 -> Block -> [Transaction] -> Difficulity -> Maybe Block
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
    

coinbaseTransaction :: [Transaction] -> Transaction
coinbaseTransaction txs =
    CoinbaseTransaction [TxOutput 50 ""]

growChain :: Db.Db -> Timer -> IO ()
growChain db timer = do
--  blocksChan <- atomically $ Db.subscribeToDbBlocks db
--  txChan <- Db.subscribeToDbTransactions db
  forever $ do
    time <- timer
    rnd <- randomIO :: IO Int64
    generated <- do
      cbtx <- coinbaseTransaction <$> Db.getTransactions db
      txs <- (:) cbtx <$> Db.getTransactions db
      (_, topBlock) <- Db.getLongestHead db
      next <- tryGenerateBlock time rnd <$> pure topBlock <*> pure txs <*> Db.getNextDifficulity db
      case next of
        Just blk -> Db.pushBlock db blk
        Nothing -> return False
    if generated
    then do
      (headLength, topBlock) <- Db.getLongestHead db
      nextDiff <- Db.getNextDifficulity db
      logInfo $ show time ++ " got chain of length " ++ show headLength
                  ++ "; block difficulity is " ++ (show $ blockDifficulity topBlock)
                  ++ "; next difficulity is " ++ (show nextDiff)
                  ++ "; blockhash is" ++ (show $ hash topBlock)
      return ()
    else return ()
