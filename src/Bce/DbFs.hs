{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Bce.DbFs
    ( Db
    , dbDataDir
    , initDb
    , unsafeCloseDb
    , loadDb
    , pushBlock
    , getBlocksFrom
    , getBlocksTo
    , getLongestHead
    , getBlock
    , getTransactions
    , getDbTransaction
    , pushTransactions
    , getNextDifficulity
    , maxCoinbaseReward
    , getPubKeyBalance
    , unspentAt
    , transactionFee
    , resolveInputOutput
    , isBlockExists
    , transactionally
    )
        where

import Bce.BlockChain
import Bce.InitialBlock    
import Bce.Hash
import Bce.BlockChainHash
import Bce.BlockChainSerialization    
import Bce.TimeStamp
import Bce.Difficulity    
import Bce.Util
import Bce.Logger
import Bce.Crypto
import Bce.Cache    

import GHC.Generics (Generic)
import GHC.Int(Int64, Int32)
import Data.IORef
import System.IO
import System.Directory    
import Data.Default
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe        
import Data.List
import Data.Ord    
import Control.Monad.IO.Class (liftIO)
import Control.Monad

import qualified Data.Set as Set
import qualified Data.Map as Map        
import qualified Control.Concurrent.RLock as Lock
import qualified Database.LevelDB.Base as LevelDb
import qualified Database.LevelDB.Internal as LevelDbInt
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Exception as Exception    
    

type Path = String
data ChainHead = ChainHead { chainHeadBlockHeader :: BlockHeader
                           , chainHeadLength :: Int
                           , chainHeadBranchedFrom :: Hash
                           , chainHeadLastUpdated :: TimeStamp } deriving (Show, Eq)

instance Ord ChainHead where
    compare l r = compare
                  (hash $ chainHeadBlockHeader l, chainHeadLength l)
                  (hash $ chainHeadBlockHeader r, chainHeadLength r)
    

data Db = Db {
      dbLock :: Lock.RLock
    , dbDataDir :: Path
    , dbTxIndex :: LevelDb.DB
    , dbBlocksIndex :: LevelDb.DB
    , dbHeads :: IORef (Set.Set ChainHead)
    , dbTransactions :: IORef (Set.Set Transaction)
    , dbUnspentCache :: Cache BlockId (Set.Set TxOutputRef)
      }


transactionally :: Db -> IO b -> IO b
transactionally db fx = Lock.with (dbLock db) fx

maxCachedUnspent = 100                        

initDb :: Path -> IO Db
initDb dataDir =  do
  createDirectoryIfMissing False dataDir
  transactionsIndexDb <- LevelDb.open (dataDir ++ "/transactions.db")
             (LevelDb.defaultOptions { LevelDb.createIfMissing = True })
  blocksIndexDb <- LevelDb.open (dataDir ++ "/blocks.db")
                  (LevelDb.defaultOptions { LevelDb.createIfMissing = True })
  withBinaryFile (blockPath dataDir $ hash initialBlock) WriteMode
                   $ (\h -> BSL.hPut h (BinPut.runPut $ Bin.put initialBlock))
  startHead <- ChainHead (blockHeader initialBlock) 1 (hash initialBlock) <$> now
  Db <$> Lock.new <*> pure dataDir <*> pure transactionsIndexDb
         <*> pure blocksIndexDb <*> newIORef (Set.singleton startHead) <*> newIORef Set.empty
         <*> createCache maxCachedUnspent

unsafeCloseDb :: Db -> IO ()
unsafeCloseDb db = do
    LevelDbInt.unsafeClose (dbBlocksIndex db)
    LevelDbInt.unsafeClose (dbTxIndex db)              
    

nextBlocks :: Db -> Hash -> IO (Set.Set Hash)
nextBlocks db prevBlockHash = do
  nextBlocksBs <- LevelDb.get (dbBlocksIndex db) def (hashBs prevBlockHash) 
  return $ fromMaybe Set.empty $ (BinGet.runGet Bin.get <$> (BSL.fromStrict <$> nextBlocksBs))

loadBlock :: Db -> Block -> IO ()
loadBlock db block = do
  pushBlockToRamState db block


loadDb :: Db -> IO ()
loadDb db =  Lock.with (dbLock db) $ do
    logInfo "loading database"
    pushBlock db initialBlock
    continue (hash initialBlock)
    head <- longestHead db
    logInfo $ "loaded blocks, longest head is " ++ show (chainHeadLength head) 
    where continue fromHash = do
            nextBlocksHashes <- Set.toList <$> nextBlocks db fromHash
            nextBlocks <- catMaybes <$> mapM (loadBlockFromDisk db) nextBlocksHashes
            logDebug $ "loading " ++ show nextBlocksHashes
            mapM_ (\nb -> loadBlock db nb)  nextBlocks
            heads <- readIORef (dbHeads db) 
            mapM_ (\b -> continue (hash  b)) nextBlocks


dbBlockPath :: Db -> Hash -> Path
dbBlockPath db blockHash = blockPath (dbDataDir db) blockHash

blockPath dataDir blockHash = dataDir ++ "/" ++ show blockHash ++ ".blk"                           


isBlockExists :: Db -> Hash -> IO Bool
isBlockExists db blockId = isJust <$> loadBlockFromDisk db blockId
                  
loadBlockFromDisk :: Db -> Hash -> IO (Maybe Block)
loadBlockFromDisk db blockHash = Lock.with (dbLock db) $ 
    Exception.catch (do
                      withBinaryFile (dbBlockPath db blockHash) ReadMode
                                         $ (\h -> do
                                              content <- BSL.fromStrict <$> BS.hGetContents h
                                              return $ Just $ BinGet.runGet Bin.get content)
                    ) (\e -> do
                         let err = show (e :: Exception.SomeException)
                         logTrace $ "error occured on block loading " ++ err
                         return Nothing
                      )


pushBlockToDisk :: Db -> Block -> IO ()
pushBlockToDisk db block = do
    withBinaryFile (dbBlockPath db $ hash block) WriteMode
                   $ (\h -> BSL.hPut h (BinPut.runPut $ Bin.put block))
    let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader $ block
    newNextBlocks <- Set.insert (hash block) <$> nextBlocks db prevBlockHash
    LevelDb.put (dbBlocksIndex db) def (hashBs prevBlockHash)
               (BSL.toStrict $ BinPut.runPut $ Bin.put newNextBlocks)
    mapM_ (\tx -> pushDbTransaction db tx block) $ blockTransactions block


chainLength :: Db -> Hash -> IO Int
chainLength db blockHash
    | blockHash == hash initialBlock = return 1
    | otherwise = do
        Just block <- loadBlockFromDisk db blockHash
        let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader block
        (+1) <$> chainLength db prevBlockHash

resolveInputOutput :: Db -> TxInput -> IO (Maybe TxOutput)
resolveInputOutput db (TxInput (TxOutputRef refTxId refOutputIdx)) =
    runMaybeT $ do
      refTx <- MaybeT $ getDbTransaction db refTxId
      liftMaybe $ (Set.toList $ txOutputs refTx) `at` (fromIntegral refOutputIdx)                          


transactionFee :: Db -> Transaction -> IO (Maybe Int64)
transactionFee _ (CoinbaseTransaction _) = return $ Just 0
transactionFee db (Transaction inputs outputs _) = runMaybeT $ do
    let totalOutput = sum $ Set.map outputAmount outputs
    inputOutputs <- liftIO $ mapM (resolveInputOutput db) (Set.toList inputs)
    totalInput <- liftMaybe $ sum <$> map outputAmount <$> (sequence inputOutputs)
    return $ totalInput - totalOutput

maxCoinbaseReward :: Db -> [Transaction] -> IO (Maybe Int64)
maxCoinbaseReward db txs = Lock.with (dbLock db) $ runMaybeT $ do
  feesOpt <- liftIO $ mapM (transactionFee db) txs
  fees <- liftMaybe $ sequence feesOpt 
  let baseReward = 50
  return $ baseReward + sum fees



consumeTransactions :: Db -> Block -> IO ()
consumeTransactions db block = do
  oldTxs <- readIORef (dbTransactions db)
  let newTxs = Set.difference oldTxs (blockTransactions block)
  writeIORef (dbTransactions db) newTxs

                       
pushBlock :: Db -> Block -> IO Bool
pushBlock db block = Lock.with (dbLock db) $ do
      logDebug $ "pushing block" ++ show (hash block)
      pushBlockToDisk db block
      consumeTransactions db block
      pushBlockToRamState db block
      return True                                                    -- TODO: smell


updateUnspentCache :: Db -> Block -> IO ()
updateUnspentCache db block = do
  prevUnspent <- unspentAt db (bhPrevBlockHeaderHash $ blockHeader block)
  let (income, spendings) = singleBlockUnspent block
  let blockUnspent = Set.difference (Set.union prevUnspent income) spendings
  const () <$> cacheValue (hash block) blockUnspent (dbUnspentCache db)
  
                       
pushBlockToRamState :: Db -> Block -> IO ()
pushBlockToRamState db block
    | block == initialBlock = return () -- already in heads! see initialization. refactor this shit
    | otherwise = do
    let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader $ block
    oldHeads <- readIORef $ dbHeads db
    let prevHeadOpt = find (\h -> prevBlockHash == (hash $ chainHeadBlockHeader h)) oldHeads
    newHeads <- case prevHeadOpt of
                  Nothing -> do
                    newHeadLength <- chainLength db (bhPrevBlockHeaderHash $ blockHeader block)
                    newHead <- ChainHead (blockHeader block) (1 + newHeadLength)
                               (bhPrevBlockHeaderHash $ blockHeader block) <$> now
                    return $ Set.insert newHead oldHeads
                  Just prevHead -> do
                    newHead <- ChainHead (blockHeader block) (1 + chainHeadLength prevHead)
                                  (chainHeadBranchedFrom prevHead) <$> now
                    let fixedHeads = Set.delete prevHead oldHeads
                    return $ Set.insert newHead fixedHeads
    writeIORef (dbHeads db) newHeads
    updateUnspentCache db block



getBlock :: Db -> Hash -> IO (Maybe Block)
getBlock db hash = Lock.with (dbLock db) $ do
    loadBlockFromDisk db hash

longestHead :: Db -> IO ChainHead
longestHead db = maximumBy (comparing chainHeadLength) <$> readIORef (dbHeads db)


getLongestHead :: Db -> IO (Int, Block)
getLongestHead db = Lock.with (dbLock db) $ do
  head <- longestHead db
  Just blk <- loadBlockFromDisk db $ hash $ chainHeadBlockHeader head
  return (chainHeadLength head, blk)
         
        
-- TODO: use next blocks and longest head (we have branches)
getBlocksFrom :: Db -> BlockId -> IO (Maybe [Block])
getBlocksFrom db fromBlockId =
    let continue bh acc
            | bh == fromBlockId = return $ Just $ reverse acc
            | otherwise = do
                 b <- loadBlockFromDisk db bh
                 case b of
                   Just block ->  do
                       let nh = bhPrevBlockHeaderHash $ blockHeader block
                       continue nh (block:acc)
                   Nothing -> return Nothing
    in Lock.with (dbLock db) $ do
      s <- hash <$> chainHeadBlockHeader <$> longestHead db
      continue s []


getBlocksTo :: Db -> BlockId -> Int -> IO [Block]               
getBlocksTo db toBlockId amount
    | amount == 0 = return []
    | toBlockId == hash initialBlock = return []
    | otherwise = do
  blockOpt <- loadBlockFromDisk db toBlockId :: IO (Maybe Block)
  case blockOpt of
    Just block -> Lock.with (dbLock db) $ do
                  let prevHash = bhPrevBlockHeaderHash $ blockHeader block
                  prevBlocks <- getBlocksTo db prevHash $ amount - 1
                  return $ block : prevBlocks
    Nothing -> return [] 
     
    
-- txs!
getTransactions :: Db -> IO (Set.Set Transaction)
getTransactions db = Lock.with (dbLock db) $ do
                       readIORef $ dbTransactions db


pushTransactions :: Db -> Set.Set Transaction -> IO ()
pushTransactions db newTransactions =
    Lock.with (dbLock db) $ do
      oldTransactions <- readIORef (dbTransactions db)
      writeIORef (dbTransactions db) (Set.union oldTransactions newTransactions)


data TransactionRef = TransactionRef { txrefBlock :: Hash
                                     , txrefIdx :: Int32} deriving (Show, Eq, Generic)
instance Bin.Binary TransactionRef

getDbTransaction :: Db -> TransactionId -> IO (Maybe Transaction)
getDbTransaction db txId = Lock.with (dbLock db) $ runMaybeT $ do
    txBin <- MaybeT $ liftIO $ LevelDb.get (dbTxIndex db) def (hashBs txId)
    let txref = BinGet.runGet Bin.get (BSL.fromStrict txBin) :: TransactionRef
    liftIO $ logInfo $ "got txRef" ++ show txref
    block <- MaybeT $  loadBlockFromDisk db (txrefBlock txref)
    liftMaybe $ (Set.toList $ blockTransactions block) `at` (fromIntegral $ txrefIdx txref)

pushDbTransaction :: Db -> Transaction -> Block -> IO ()
pushDbTransaction db tx block =
    let Just txIdx = Set.lookupIndex tx (blockTransactions block)
        txRef = TransactionRef (hash block) $ fromIntegral txIdx
    in LevelDb.put (dbTxIndex db) def (hashBs $ transactionId block tx)
           (BSL.toStrict $ BinPut.runPut $ Bin.put $ txRef)


lastNBlocks :: Db -> Int -> IO [Block]
lastNBlocks db n = do
    upto <- hash <$> chainHeadBlockHeader <$> longestHead db
    getBlocksTo db upto n


getNextDifficulity :: Db -> IO Difficulity
getNextDifficulity db = Lock.with (dbLock db) $ do
  blocks <- lastNBlocks db difficulityRecalculationBlocks
  return $ nextDifficulity blocks


singleBlockUnspent :: Block -> (Set.Set TxOutputRef, Set.Set TxOutputRef)
singleBlockUnspent block = 
    let
        txs = blockTransactions block
        income = Set.fromList $ concatMap
                 (\tx ->
                      let enumeratedOutputs = (zip (Set.toList $ txOutputs tx) [0..])
                      in map (\(o, n) -> TxOutputRef (transactionId block tx) n) enumeratedOutputs) txs
        spendings = Set.fromList $ concatMap
                    (\tx -> case tx of
                              CoinbaseTransaction _ -> []
                              _ -> Set.toList $ Set.map inputOutputRef $ txInputs tx) txs
    in (income, spendings)

unspentAt :: Db -> Hash -> IO (Set.Set TxOutputRef)
unspentAt db uptoBlock =
    continue uptoBlock Set.empty Set.empty
    where continue h unspent spent
              | h == hash initialBlock = let (income, spendings) = singleBlockUnspent initialBlock
                                         in return (Set.difference (Set.union income unspent)
                                                                   (Set.union spent spendings))
              | otherwise = do
            cached <- queryCache h (dbUnspentCache db)
            case cached of
              Just val -> return (Set.difference (Set.union unspent val) spent)
              Nothing -> do
                          Just block <- loadBlockFromDisk db h
                          let txs = blockTransactions block
                          let (income, spendings) = singleBlockUnspent block
                          let newUnspent = Set.difference (Set.union unspent income) (Set.union spent spendings)
                          let newSpent = Set.difference (Set.union spent spendings) (Set.union unspent income)
                          continue (bhPrevBlockHeaderHash $ blockHeader block) newUnspent newSpent


balanceAt :: Db -> Hash -> PubKey -> IO (Set.Set TxOutputRef)
balanceAt db uptoBlock ownerKey = do
    allUnspentOutputs <- unspentAt db uptoBlock
    Set.fromList
           <$> (filterM (\ref -> do
                           Just output <- resolveInputOutput db (TxInput ref)
                           return $ ownerKey == outputPubKey output
                        ) $ Set.toList allUnspentOutputs)


getPubKeyBalance :: Db -> PubKey -> IO (Set.Set TxOutputRef)
getPubKeyBalance db ownerKey = Lock.with (dbLock db) $ do
    uptoBlock <- hash <$> chainHeadBlockHeader <$> longestHead db
    balanceAt db uptoBlock ownerKey


-- kill unperspective forks
headTtl = round $ secondsPerBlock * 100

prune :: Db -> IO ()
prune db = Lock.with (dbLock db) $ do
             thisMoment <- now
             deathRow <- Set.filter (\h -> thisMoment - (chainHeadLastUpdated h) > headTtl)
                            <$> (readIORef $ dbHeads db)
             longestHead <- longestHead db -- have mercy on the longest
             forM_ (Set.delete longestHead deathRow) (pruneHead db)

pruneHead :: Db -> ChainHead -> IO ()
pruneHead db deadHead = return () -- TODO: implement me
