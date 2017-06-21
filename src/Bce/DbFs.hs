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
    , pushTransactions
    , getNextDifficulity)
        where


import Bce.BlockChain
import Bce.InitialBlock    
import Bce.Hash
import Bce.BlockChainHash
import Bce.BlockChainSerialization    
import Bce.TimeStamp
import Bce.Difficulity    
import Bce.Util    


import Data.IORef
import System.IO
import System.Directory    
import Data.Default
import Data.Maybe
import Data.Either    
import Control.Monad.Trans.Either    
import Data.List
import Data.Ord    
import Control.Monad.IO.Class (liftIO)
import Control.Monad    
import qualified Control.Concurrent.Lock as Lock
import qualified Database.LevelDB.Base as LevelDb
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Exception as Exception    

    

type Path = String

data Index = Index

data ChainHead = ChainHead { chainHeadBlockHeader :: BlockHeader
                           , chainHeadLength :: Int } deriving (Show, Eq)
    

data Db = Db {
      dbLock :: Lock.Lock
    , dbDataDir :: Path
    , dbTxIndex :: LevelDb.DB
    , dbBlocksIndex :: LevelDb.DB
    , dbHeads :: IORef [ChainHead]
    , dbTransactions :: IORef [Transaction]
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
  
  let startHead = ChainHead (blockHeader initialBlock) 1
  Db <$> Lock.new <*> pure dataDir <*> pure transactionsIndexDb
         <*> pure blocksIndexDb <*> newIORef [startHead] <*> newIORef []


nextBlocks :: Db -> Hash -> IO [Hash]
nextBlocks db prevBlockHash = do
  nextBlocksBs <- LevelDb.get (dbBlocksIndex db) def (hashBs prevBlockHash) 
  return $ fromMaybe [] $ (BinGet.runGet Bin.get <$> (BSL.fromStrict <$> nextBlocksBs))
      

loadDb :: Db -> IO ()
loadDb db =  Lock.with (dbLock db) $ do
    pushBlockNoLock db initialBlock
    continue (hash initialBlock)
    where continue fromHash = do
            nextBlocksHashes <- nextBlocks db fromHash
            nextBlocks <- catMaybes <$> mapM (loadBlockFromDisk db) nextBlocksHashes
            putStrLn $ "loading " ++ show nextBlocksHashes
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
--                         putStrLn $ "error occured on block loading" ++ err
                         return Nothing
                      )


pushBlockToDisk :: Db -> Block -> IO ()
pushBlockToDisk db block = do
    withBinaryFile (dbBlockPath db $ hash block) WriteMode
                   $ (\h -> BSL.hPut h (BinPut.runPut $ Bin.put block))
    let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader $ block
    newNextBlocks <- (:) (hash block) <$> nextBlocks db prevBlockHash
    LevelDb.put (dbBlocksIndex db) def (hashBs prevBlockHash)
               (BSL.toStrict $ BinPut.runPut $ Bin.put newNextBlocks)
    -- TODO: transactions index!


chainLength :: Db -> Hash -> IO Int
chainLength db blockHash
    | blockHash == hash initialBlock = return 1
    | otherwise = do
        Just block <-  loadBlockFromDisk db blockHash
        let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader block
        (+1) <$> chainLength db prevBlockHash


-- TODO: move verification away from here!

verifyPrevBlockHashCorrect db block = do
  let prevHash = bhPrevBlockHeaderHash $ blockHeader block
  prevBlockOpt <- liftIO $ loadBlockFromDisk db prevHash
  guard (isJust prevBlockOpt) `mplus` left "wrong prev block hash"


verifyBlockDifficulity db block = do
  expectedDifficulity <- liftIO $ getNextDifficulityNoLock db
  let actualDifficulity = blockDifficulity block
  let stampedDifficulity = fromIntegral $ bhDifficulity $ blockHeader block
  guard (stampedDifficulity == expectedDifficulity) `mplus` left "wrong stamped difficulity"
  guard (actualDifficulity >= expectedDifficulity) `mplus` left "wrong difficulity" 


verifyBlock :: Db -> Block -> EitherT String IO [()]
verifyBlock db block = sequence
                       [ verifyPrevBlockHashCorrect db block
                       , verifyBlockDifficulity db block]


--- end of verification, move is somewhere elso!
  
  

pushBlocks :: Db -> [Block] -> IO ()
pushBlocks db blocks = mapM_ (pushBlock db) (reverse blocks) -- starting from oldest

pushBlock :: Db -> Block -> IO Bool
pushBlock db block = Lock.with (dbLock db) $ do
                           pushBlockToDisk db block
                           pushBlockNoLock db block
                       
pushBlockNoLock :: Db -> Block -> IO Bool
pushBlockNoLock db block =  do
  isValidBlock <- isRight <$> (runEitherT $ verifyBlock db block)
  if isValidBlock 
  then do
    let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader $ block
    oldHeads <- readIORef $ dbHeads db
    let prevHeadOpt = find (\h -> prevBlockHash == (hash $ chainHeadBlockHeader h)) oldHeads
    newHeads <- case prevHeadOpt of
                  Nothing -> do
                    newHead <- ChainHead (blockHeader block)
                               <$> chainLength db (bhPrevBlockHeaderHash $ blockHeader block)
                    return $ newHead : oldHeads
                  Just prevHead -> do
                    let newHead = ChainHead (blockHeader block) (1 + chainHeadLength prevHead)
                    let fixedHeads = delete prevHead oldHeads
                    return $ newHead : fixedHeads
    writeIORef (dbHeads db) newHeads
    return True
  else return False


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
    

-- kill unperspective forks
prune :: Db -> IO ()
prune db = return () -- TODO: implement me


-- txs!
getTransactions :: Db -> IO [Transaction]
getTransactions db = Lock.with (dbLock db) $ do
                       readIORef $ dbTransactions db


pushTransactions :: Db -> [Transaction] -> IO ()
pushTransactions db newTransactions = Lock.with (dbLock db) $ do
                        oldTransactions <- readIORef (dbTransactions db)
                        writeIORef (dbTransactions db) (oldTransactions ++ newTransactions)


lastNBlocks :: Db -> Int -> IO [Block]
lastNBlocks db n = do
  let border = hash $ initialBlock
  start <- hash <$> chainHeadBlockHeader <$> longestHead db
  let continue s k bs
          | s == border = return $ reverse bs
          | k == 0 = return $ reverse bs
          | otherwise = do
                     Just block <- loadBlockFromDisk db s
                     let nxt = bhPrevBlockHeaderHash $ blockHeader block
                     continue nxt (k -1) (block:bs)
  continue start n []

getNextDifficulity :: Db -> IO Difficulity           
getNextDifficulity db = Lock.with (dbLock db) $ getNextDifficulityNoLock db
           
getNextDifficulityNoLock :: Db -> IO Difficulity
getNextDifficulityNoLock db =  do
  blocks <- lastNBlocks db difficulityRecalculationBlocks
  return $ nextDifficulity blocks
    
