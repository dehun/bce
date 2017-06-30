{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Bce.DbFs
    ( initDb
    , loadDb
    , pushBlock
    , pushBlocks
    , getBlocksFromHash
    , getLongestHead
    , getBlock
    , Db
    , getTransactions
    , getDbTransaction
    , pushTransactions
    , getNextDifficulity
    , maxCoinbaseReward
    , getPubKeyBalance)
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
import qualified Control.Concurrent.Lock as Lock
import qualified Database.LevelDB.Base as LevelDb
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
      dbLock :: Lock.Lock
    , dbDataDir :: Path
    , dbTxIndex :: LevelDb.DB
    , dbBlocksIndex :: LevelDb.DB
    , dbHeads :: IORef (Set.Set ChainHead)
    , dbTransactions :: IORef (Set.Set Transaction)
      }


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


nextBlocks :: Db -> Hash -> IO (Set.Set Hash)
nextBlocks db prevBlockHash = do
  nextBlocksBs <- LevelDb.get (dbBlocksIndex db) def (hashBs prevBlockHash) 
  return $ fromMaybe Set.empty $ (BinGet.runGet Bin.get <$> (BSL.fromStrict <$> nextBlocksBs))
      

loadDb :: Db -> IO ()
loadDb db =  Lock.with (dbLock db) $ do
    logInfo "loading database"
    pushBlockNoLock db initialBlock
    continue (hash initialBlock)
    head <- longestHead db
    logInfo $ "loaded blocks, longest head is " ++ show (chainHeadLength head) 
    where continue fromHash = do
            nextBlocksHashes <- Set.toList <$> nextBlocks db fromHash
            nextBlocks <- catMaybes <$> mapM (loadBlockFromDisk db) nextBlocksHashes
            logDebug $ "loading " ++ show nextBlocksHashes
            mapM_ (\nb -> pushBlockNoLock db nb)  nextBlocks
            heads <- readIORef (dbHeads db) 
            mapM_ (\b -> continue (hash  b)) nextBlocks


dbBlockPath :: Db -> Hash -> Path
dbBlockPath db blockHash = blockPath (dbDataDir db) blockHash

blockPath dataDir blockHash = dataDir ++ "/" ++ show blockHash ++ ".blk"                           

                  
loadBlockFromDisk :: Db -> Hash -> IO (Maybe Block)
loadBlockFromDisk db blockHash =
    Exception.catch (do
                      withBinaryFile (dbBlockPath db blockHash) ReadMode
                                         $ (\h -> do
                                              content <- BSL.fromStrict <$> BS.hGetContents h
                                              return $ Just $ BinGet.runGet Bin.get content)
                    ) (\e -> do
                         let err = show (e :: Exception.IOException)
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

isCoinbaseTransaction :: Transaction -> Bool    
isCoinbaseTransaction (CoinbaseTransaction _) = True
isCoinbaseTransaction _ = False

resolveInputOutput :: Db -> TxInput -> IO (Maybe TxOutput)
resolveInputOutput db (TxInput (TxOutputRef refTxId refOutputIdx)) =
    runMaybeT $ do
      refTx <- MaybeT $ getDbTransactionNoLock db refTxId
      liftMaybe $ (Set.toList $ txOutputs refTx) `at` (fromIntegral refOutputIdx)                          


transactionFee :: Db -> Transaction -> IO (Maybe Int64)
transactionFee _ (CoinbaseTransaction _) = return $ Just 0
transactionFee db (Transaction inputs outputs _) = runMaybeT $ do
    let totalOutput = sum $ Set.map outputAmount outputs
    inputOutputs <- liftIO $ mapM (resolveInputOutput db) (Set.toList inputs)
    totalInput <- liftMaybe $ sum <$> map outputAmount <$> (sequence inputOutputs)
    return $ totalInput - totalOutput

maxCoinbaseReward :: Db -> [Transaction] -> IO (Maybe Int64)
maxCoinbaseReward db txs = Lock.with (dbLock db) $ maxCoinbaseRewardNoLock db txs                          

maxCoinbaseRewardNoLock :: Db -> [Transaction] -> IO (Maybe Int64)
maxCoinbaseRewardNoLock db txs = runMaybeT $ do
  feesOpt <- liftIO $ mapM (transactionFee db) txs
  fees <- liftMaybe $ sequence feesOpt 
  let baseReward = 50
  return $ baseReward + sum fees

           


-- TODO: move verification away from here!

verifyPrevBlockHashCorrect db block = do
  let prevHash = bhPrevBlockHeaderHash $ blockHeader block
  prevBlockOpt <- liftIO $ loadBlockFromDisk db prevHash
  guard (isJust prevBlockOpt) `mplus` left "wrong prev block hash"


verifyBlockDifficulity db block = do
  let prevHash = (bhPrevBlockHeaderHash $ blockHeader block)
  prevBlocks <- liftIO $ getBlocksToHash db prevHash difficulityRecalculationBlocks
  let expectedDifficulity = nextDifficulity prevBlocks
  let actualDifficulity = blockDifficulity block
  let stampedDifficulity = fromIntegral $ bhDifficulity $ blockHeader block
  guard (stampedDifficulity == expectedDifficulity) `mplus` left "wrong stamped difficulity"
  guard (actualDifficulity >= expectedDifficulity) `mplus` left "wrong difficulity"


blocksForTimeAveraging = 10                         
verifyBlockTimestamp db block = do
  let blockTimestamp  = bhWallClockTime . blockHeader
  lastBlocks <- liftIO $ getBlocksToHash db (hash block) blocksForTimeAveraging
  case lastBlocks of
    [] -> return ()
    _ -> do
      let avgTime = median $ map blockTimestamp lastBlocks
      guard (blockTimestamp block >= avgTime) `mplus` left "block timestamp is incorrect, less than last avg"


verifyTransactionSignature db tx =
    case tx of
      Transaction inputs outputs sig -> do
              inputOutputsOptSeq <- liftIO $ mapM (resolveInputOutput db) (Set.toList inputs)
              let inputOutputsOpt = sequence inputOutputsOptSeq
              guard (isJust inputOutputsOpt)
                        `mplus` left "can not resolve input's output for transaction"
              let inputOutputs = fromJust inputOutputsOpt
              let pubKey = outputPubKey $ head inputOutputs
              guard (all (\o -> pubKey == outputPubKey o) inputOutputs)
                        `mplus` left "all input pub keys should match"
              guard (verifySignature sig pubKey (hash tx)) `mplus` left "transaction signature is incorrect"

verifyTransactionTransaction db tx@(Transaction inputs outputs sig) = do
  fee <- liftIO $ transactionFee db tx
  guard (isJust fee) `mplus` left "transaction fee can not be calculated (absent input?)"
  guard (fromJust fee >= 0) `mplus` left "transaction fee is below zero"
  guard (length inputs > 0) `mplus` left "there are should be at least one transaction input"
  guard (length outputs > 0) `mplus` left "there are should be at least one transaction output"
  verifyTransactionSignature db tx
verifyTransactionTransaction db _ = left "coinbase transaction is not allowed"
                             

verifyTransaction db block tx = 
  case tx of
    CoinbaseTransaction outputs -> do
            guard (onlyOne isCoinbaseTransaction $ Set.toList $ blockTransactions block)
                      `mplus` left "more than one coinbase per block"
            guard (1 == length outputs)
                      `mplus` left "more than one output in coinbase transaction"
            expectedCoinbaseReward <- liftIO $ maxCoinbaseRewardNoLock db (Set.toList $ blockTransactions block)
            guard (isJust expectedCoinbaseReward) `mplus` left "can not calculate or wrong coinbase reward"
            guard ((outputAmount $ head $ Set.toList $ outputs) == fromJust expectedCoinbaseReward)
                      `mplus` left "coinbase reward is incorrectly stamped"
    Transaction inputs outputs sig -> verifyTransactionTransaction db tx


verifyBlockTransactions db block = do
  let txs = blockTransactions block
  guard (hash txs == bhTransactionsHash (blockHeader block)) `mplus` left "wrong stamped transactions hash"
  let allInputs = concatMap (\tx -> case tx of
                                          CoinbaseTransaction _ -> []
                                          _ -> Set.toList $ txInputs tx
                            ) $ Set.toList txs
  guard (all (\inp -> onlyOne (==inp) allInputs) allInputs) `mplus` left "input used more than once"
  mapM_ (\tx -> verifyTransaction db block tx
                `mplus` (left $ "; in transaction" ++ show (hash tx))) txs


verifyBlock :: Db -> Block -> EitherT String IO [()]
verifyBlock db block = do
    sequence [ verifyPrevBlockHashCorrect db block
             , verifyBlockDifficulity db block
             , verifyBlockTimestamp db block
             , verifyBlockTransactions db block] `mplus` (left $ "; in block" ++ (show $ hash block))

--- end of verification, move is somewhere elso!

pushBlocks :: Db -> [Block] -> IO ()
pushBlocks db blocks = mapM_ (pushBlock db) (reverse blocks) -- starting from oldest

pushBlock :: Db -> Block -> IO Bool
pushBlock db block = Lock.with (dbLock db) $ pushBlockNoLock db block


consumeTransactions :: Db -> Block -> IO ()
consumeTransactions db block = do
  oldTxs <- readIORef (dbTransactions db)
  let newTxs = Set.difference oldTxs (blockTransactions block)
  writeIORef (dbTransactions db) newTxs

                       
pushBlockNoLock :: Db -> Block -> IO Bool
pushBlockNoLock db block = do
                       verificationResult <- runEitherT $ verifyBlock db block
                       case  verificationResult of
                         Right _ -> do
                             pushBlockToDisk db block
                             consumeTransactions db block
                             pushBlockToHeads db block
                             return True
                         Left err -> do
                             logWarning $ "verification  pushing block failed: " ++ err
                             return False
                       
pushBlockToHeads :: Db -> Block -> IO ()
pushBlockToHeads db block =  do
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



getBlock :: Db -> Hash -> IO (Maybe Block)
getBlock db hash = Lock.with (dbLock db) $ do
    loadBlockFromDisk db hash


loadTransaction :: Db -> Hash -> IO Transaction
loadTransaction db txHash = undefined


longestHead :: Db -> IO ChainHead
longestHead db = maximumBy (comparing chainHeadLength) <$> readIORef (dbHeads db)


getLongestHead :: Db -> IO (Int, Block)
getLongestHead db = Lock.with (dbLock db) $ do
  head <- longestHead db
  Just blk <- loadBlockFromDisk db $ hash $ chainHeadBlockHeader head
  return (chainHeadLength head, blk)
         
        
-- TODO: use next blocks and longest head (we have branches)
getBlocksFromHash :: Db -> Hash -> IO (Maybe [Block])
getBlocksFromHash db fromHash =
    let continue bh acc
            | bh == fromHash = return $ Just $ reverse acc
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

getBlocksToHash :: Db -> Hash -> Int -> IO [Block]               
getBlocksToHash db toHash amount
    | amount == 0 = return []
    | toHash == hash initialBlock = return []
    | otherwise = do
  blockOpt <- loadBlockFromDisk db toHash :: IO (Maybe Block)
  case blockOpt of
    Just block -> do
                  let prevHash = bhPrevBlockHeaderHash $ blockHeader block
                  prevBlocks <- getBlocksToHash db prevHash (amount - 1)
                  return (block : prevBlocks)
    Nothing -> return [] 
     
    
-- txs!
getTransactions :: Db -> IO (Set.Set Transaction)
getTransactions db = Lock.with (dbLock db) $ do
                       readIORef $ dbTransactions db


pushTransactions :: Db -> Set.Set Transaction -> IO (Either String ())
pushTransactions db newTransactions =
    Lock.with (dbLock db) $ runEitherT $ do
      mapM (verifyTransactionTransaction db) $ Set.toList newTransactions
      oldTransactions <- liftIO $ readIORef (dbTransactions db)
      liftIO $ writeIORef (dbTransactions db) (Set.union oldTransactions newTransactions)


data TransactionRef = TransactionRef { txrefBlock :: Hash
                                     , txrefIdx :: Int32} deriving (Show, Eq, Generic)
instance Bin.Binary TransactionRef

getDbTransaction :: Db -> Hash -> IO (Maybe Transaction)
getDbTransaction db txHash = Lock.with (dbLock db) $ getDbTransactionNoLock db txHash
    
getDbTransactionNoLock :: Db -> Hash -> IO (Maybe Transaction)
getDbTransactionNoLock db txHash =
  runMaybeT $ do
    txBin <- MaybeT $ liftIO $ LevelDb.get (dbTxIndex db) def (hashBs txHash)
    let txref = BinGet.runGet Bin.get (BSL.fromStrict txBin) :: TransactionRef
    block <- MaybeT $  loadBlockFromDisk db (txrefBlock txref)
    liftMaybe $ (Set.toList $ blockTransactions block) `at` (fromIntegral $ txrefIdx txref)

pushDbTransaction :: Db -> Transaction -> Block -> IO ()
pushDbTransaction db tx block =
    LevelDb.put (dbTxIndex db) def (hashBs $ hash tx)
               (BSL.toStrict $ BinPut.runPut $ Bin.put $ hash block)


lastNBlocks :: Db -> Int -> IO [Block]
lastNBlocks db n = do
    upto <- hash <$> chainHeadBlockHeader <$> longestHead db
    getBlocksToHash db upto n


getNextDifficulity :: Db -> IO Difficulity
getNextDifficulity db = Lock.with (dbLock db) $ getNextDifficulityNoLock db
           
getNextDifficulityNoLock :: Db -> IO Difficulity
getNextDifficulityNoLock db =  do
  blocks <- lastNBlocks db difficulityRecalculationBlocks
  return $ nextDifficulity blocks


balanceAt :: Db -> Hash -> PubKey -> IO (Set.Set TxOutputRef)
balanceAt db uptoBlock ownerKey = undefined


getPubKeyBalance :: Db -> PubKey -> IO (Set.Set TxOutputRef)
getPubKeyBalance db ownerKey = do
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
  
  
         
