module Bce.Db where

import Bce.BlockChain
import Bce.BlockChainVerification
import Bce.InitialBlock    
import Bce.Hash
import Bce.Difficulity    
import Bce.BlockChainHash

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

data Db = Db {
      dbBlockChain :: TVar BlockChain
    , dbTransactions :: TVar [Transaction]
    , dbBlocksChan :: TChan Block
    }


initialBlockChain = BlockChain [ initialBlock ]

newDb :: IO Db        
newDb = Db <$> newTVarIO initialBlockChain <*> newTVarIO [] <*> newTChanIO

getTopBlock :: Db -> STM Block
getTopBlock db =
     do
      chain <- readTVar $ dbBlockChain db
      return $ head $ blockChainBlocks chain

getChainLength :: Db -> STM Int
getChainLength db = (length . blockChainBlocks) <$> readTVar (dbBlockChain db)

getNextDifficulity :: Db -> STM Difficulity             
getNextDifficulity db =
     do
      chain <- readTVar $ dbBlockChain db
      return $ nextDifficulity chain

subscribeToDbBlocks :: Db -> STM (TChan Block)
subscribeToDbBlocks db =
     dupTChan $ dbBlocksChan db


growChain :: Db -> Block -> STM Bool
growChain db newBlock = 
     do
      oldChain <- readTVar $ dbBlockChain db
      let newChain = BlockChain $ newBlock : blockChainBlocks oldChain
      if verifyBlockChain newChain
      then do
        writeTVar (dbBlockChain db) newChain
        writeTChan (dbBlocksChan db) newBlock
        return True
      else return False


regrowChain :: Db -> Hash -> [Block] -> IO Bool
regrowChain db prevBlockHash blocks = undefined

pushTransaction :: Db -> Transaction -> IO Bool
pushTransaction = undefined

getTransactions :: Db -> STM [Transaction]
getTransactions db = readTVar $ dbTransactions db
    
      
    


    
