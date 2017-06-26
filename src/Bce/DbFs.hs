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
import Bce.Logger


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
         <*> pure blocksIndexDb <*> newIORef (Set.fromList [startHead]) <*> newIORef Set.empty


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
    -- TODO: transactions index!


chainLength :: Db -> Hash -> IO Int
chainLength db blockHash
    | blockHash == hash initialBlock = return 1
    | otherwise = do
        Just block <- loadBlockFromDisk db blockHash
        let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader block
        (+1) <$> chainLength db prevBlockHash


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
  avgTime <- liftIO $ median <$> map blockTimestamp <$> lastNBlocks db blocksForTimeAveraging
  guard (blockTimestamp block > avgTime) `mplus` left ""


-- TODO: implement me        
verifyBlockTransactions db block =  return ()

-- TODO: implement me                                    
verifyBlockHasCorrectCoinbaseTransaction db block = return ()


verifyBlock :: Db -> Block -> EitherT String IO [()]
verifyBlock db block = do
    sequence [ verifyPrevBlockHashCorrect db block
             , verifyBlockDifficulity db block
             , verifyBlockTimestamp db block
             , verifyBlockHasCorrectCoinbaseTransaction db block
             , verifyBlockTransactions db block]


--- end of verification, move is somewhere elso!
  
  

pushBlocks :: Db -> [Block] -> IO ()
pushBlocks db blocks = mapM_ (pushBlock db) (reverse blocks) -- starting from oldest

pushBlock :: Db -> Block -> IO Bool
pushBlock db block = Lock.with (dbLock db) $ pushBlockNoLock db block
                       
pushBlockNoLock :: Db -> Block -> IO Bool
pushBlockNoLock db block = do
                       verificationResult <- runEitherT $ verifyBlock db block
                       case  verificationResult of
                         Right _ -> do
                           pushBlockToDisk db block
                           pushBlockToHeads db block
                         Left err -> do
                             logWarning $ "verification  pushing block failed: " ++ err
                             return False
                       
pushBlockToHeads :: Db -> Block -> IO Bool
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
    return True


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


pushTransactions :: Db -> Set.Set Transaction -> IO ()
pushTransactions db newTransactions = Lock.with (dbLock db) $ do
                        oldTransactions <- readIORef (dbTransactions db)
                        writeIORef (dbTransactions db) (Set.union oldTransactions newTransactions)


--                                    


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
  
  
         
