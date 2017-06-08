module Bce.Db where

import Bce.BlockChain
import Bce.BlockChainVerification
import Bce.InitialBlock    
import Bce.Hash
import Bce.Difficulity    
import Bce.BlockChainHash

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.List    

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

blocksFromHash :: Db -> Hash -> STM (Maybe [Block])
blocksFromHash db hash = undefined
    

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


regrowChain :: Db -> [Block] -> STM Bool
regrowChain db blocks = undefined

getBlock :: Db -> Hash -> STM (Maybe Block)
getBlock = undefined

-- transactions                        

pushTransaction :: Db -> Transaction -> STM Bool
pushTransaction = undefined

getTransactions :: Db -> STM [Transaction]
getTransactions db = readTVar $ dbTransactions db
    
      
    


    
