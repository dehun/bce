module Bce.Miner where

import Bce.Crypto
import Bce.Hash
import Bce.BlockChain
import Bce.BlockChainHash    
import Bce.BlockChainVerification
import Bce.InitialBlock
import Bce.Difficulity
import qualified Bce.Db as Db

import Data.Either    
import Debug.Trace
import GHC.Int (Int64)
import System.Random    
    
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Control.Monad
import Control.Concurrent    
import Control.Concurrent.STM    


tryGenerateBlock :: Int -> Int64 -> Block -> [Transaction] -> Difficulity -> Maybe Block
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

growChain :: Db.Db -> IO ()
growChain db = do
--  blocksChan <- atomically $ Db.subscribeToDbBlocks db
--  txChan <- Db.subscribeToDbTransactions db
  forever $ do
    time <- round <$> getPOSIXTime
    rnd <- randomIO :: IO Int64
    generated <- atomically $ do
      cbtx <- coinbaseTransaction <$> Db.getTransactions db
      txs <- (:) cbtx <$> Db.getTransactions db
      next <- tryGenerateBlock time rnd <$> Db.getTopBlock db <*> pure txs <*> Db.getNextDifficulity db
      case next of
        Just blk -> Db.growChain db blk
        Nothing -> return False
    if generated
    then do
        (bcLength, nextDiff, topBlock) <-
            atomically $ ((,,) <$> Db.getChainLength db <*> Db.getNextDifficulity db <*> Db.getTopBlock db)
        putStrLn $ show time ++ " got chain of length " ++ (show bcLength)
                  ++ "; block difficulity is " ++ (show $ blockDifficulity topBlock)
                  ++ "; next difficulity is " ++ (show nextDiff)
        return ()
    else return ()
