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
    }


initialBlockChain = BlockChain [ initialBlock ]

newDb :: IO Db        
newDb = Db <$> newTVarIO initialBlockChain <*> newTVarIO [] 

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

growChain :: Db -> Block -> STM Bool
growChain db newBlock = 
     do
      oldChain <- readTVar $ dbBlockChain db
      let newChain = BlockChain $ newBlock : blockChainBlocks oldChain
      if verifyBlockChain newChain
      then do
        writeTVar (dbBlockChain db) newChain
--        writeTChan (dbBlocksChan db) newBlock
        return True
      else return False

blocksFromHash :: Db -> Hash -> STM (Maybe [Block])
blocksFromHash db needle = do
  blocks <- blockChainBlocks <$> (readTVar $ dbBlockChain db)
  case takeWhile (\b -> hash b /= needle) blocks of
    [] -> return Nothing
    xs -> return $ Just xs
           

regrowChain :: Db -> [Block] -> STM Bool
regrowChain db blocks = do
    oldBlocks <- blockChainBlocks <$> (readTVar $ dbBlockChain db)
    let startBlock = last blocks
    let startBlockPrevHash = bhPrevBlockHeaderHash $ blockHeader $ startBlock
    let newBlocksPrefix = dropWhile (\b -> startBlockPrevHash /= hash b) oldBlocks
    let newBlockChain = BlockChain $ blocks ++ newBlocksPrefix
    if and [verifyBlockChain newBlockChain,  length (blockChainBlocks newBlockChain) > length oldBlocks]
    then do
        writeTVar (dbBlockChain db) newBlockChain
        return True
    else return False
    
getBlock :: Db -> Hash -> STM (Maybe Block)
getBlock db needle = do
  blocks <- blockChainBlocks <$> readTVar (dbBlockChain db)
  return $ find (\b -> needle == hash b) blocks

-- transactions                        

pushTransaction :: Db -> Transaction -> STM Bool
pushTransaction = undefined

getTransactions :: Db -> STM [Transaction]
getTransactions db = readTVar $ dbTransactions db
    
      
    


    
