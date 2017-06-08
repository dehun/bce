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


-- randomBlock :: Int -> Int -> Block -> Difficulity -> Block
-- randomBlock time rnd prevBlock difficulity =
--     let transactions = [CoinbaseTransaction [TxOutput 0 ""]]
--         header = BlockHeader
--                  (hash transactions)
--                  (hash $ blockHeader prevBlock)
--                  (fromIntegral rnd)
--                  (fromIntegral time)
--                  (fromIntegral difficulity)
--     in Block header transactions

    
-- growChain :: Int -> BlockChain -> IO BlockChain
-- growChain rnd oldchain = do
--   let (BlockChain blocks) = oldchain
--   nextTime <- fmap round getPOSIXTime
--   let nextblock = randomBlock nextTime rnd (head blocks) (nextDifficulity oldchain)
--   let newChain = BlockChain (nextblock : blocks)
--   if verifyBlockChain newChain
--   then do
--     putStrLn $ show nextTime ++ " got chain of length " ++ show (length (blockChainBlocks newChain))
--                  ++ "; used rnd"
--                  ++ "; block difficulity is " ++ (show (blockDifficulity (head $ blockChainBlocks newChain)))
--                  ++ "; next difficulity is " ++ (show $ nextDifficulity newChain)
--                  ++ "; growth speed is " ++ (show $ growthSpeed newChain)
--     return newChain
--   else growChain (rnd + 1) (BlockChain blocks)

type BlockGenerator = (Block -> [Transaction] -> Difficulity -> IO Block)

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
    

-- generateBlock :: Int -> Int -> Difficulity -> [Transaction] -> Difficulity -> EitherT BlockGenerator IO Block 
-- generateBlock rnd txs =

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
    else return ()

main = do
  db <- Db.newDb
  forkIO $ growChain db
  forkIO $ growChain db
  growChain db                  
