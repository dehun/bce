module DbFs where


import Bce.BlockChain
import Bce.InitialBlock    
import Bce.Hash
import Bce.BlockChainHash
import Bce.BlockChainSerialization    
import Bce.TimeStamp    


import System.IO
import System.Directory    
import Data.Default
import Data.Maybe    
import Control.Monad.IO.Class (liftIO)
import Control.Monad    
import qualified Control.Concurrent.Lock as Lock    
import qualified Database.LevelDB as LevelDb
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
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
    , dbHeads :: [ChainHead]
    , dbTransactions :: [Transaction]
      }


initDb :: Path -> IO Db
initDb dataDir =  do
  createDirectoryIfMissing False dataDir
  let transactionsIndexDb = LevelDb.runResourceT $LevelDb.open (dataDir ++ "/transactions.db")
             (LevelDb.defaultOptions { LevelDb.createIfMissing = True })
  let blocksIndexDb = LevelDb.runResourceT $LevelDb.open (dataDir ++ "/blocks.db")
                  (LevelDb.defaultOptions { LevelDb.createIfMissing = True })             
  Db <$> Lock.new <*> pure dataDir <*> transactionsIndexDb
         <*> blocksIndexDb <*> pure [] <*> pure []

nextBlocks :: Db -> Hash -> IO [Hash]
nextBlocks db prevBlockHash = do
  nextBlocksBs <- LevelDb.get (dbBlocksIndex db) def (hashBs prevBlockHash) 
  return $ fromMaybe [] $ (BinGet.runGet Bin.get <$> (BSL.fromStrict <$> nextBlocksBs))
      

loadDb :: Db -> IO Db
loadDb db' =  do
    withInitialBlockDb <- (pushBlock db' initialBlock)
    continue withInitialBlockDb (hash initialBlock)
    where continue cdb fromHash = do
            nb <- nextBlocks cdb fromHash
            ndb <- foldM (\db bh -> pushBlock db =<< loadBlockFromDisk db bh) cdb nb
            foldM (\ndb' b -> continue ndb' b) ndb nb 

blockPath :: Db -> Hash -> Path
blockPath db blockHash =  dbDataDir db ++ "/" ++ show blockHash ++ "blk"
                  
loadBlockFromDisk :: Db -> Hash -> IO Block
loadBlockFromDisk db blockHash = do
  withBinaryFile (blockPath db blockHash) ReadMode
                     $ (\h -> do
                          content <- BSL.hGetContents h
                          return $ BinGet.runGet Bin.get content)

pushBlockToDisk :: Db -> Block -> IO ()
pushBlockToDisk db block = do 
    withBinaryFile (blockPath db $ hash block) WriteMode
                   $ (\h -> BSL.hPut h (BinPut.runPut $ Bin.put block))
    let prevBlockHash = bhPrevBlockHeaderHash $ blockHeader $ block
    newNextBlocks <- (:) (hash block) <$> nextBlocks db prevBlockHash
    LevelDb.put (dbBlocksIndex db) def (hashBs prevBlockHash)
               (BSL.toStrict $ BinPut.runPut $ Bin.put newNextBlocks)
    -- TODO: transactions index!


pushBlock :: Db -> Block -> IO Db
pushBlock db block = undefined



loadTransaction :: Db -> Hash -> IO Transaction
loadTransaction db txHash = undefined


getBlocksFromHash :: Db -> Hash -> IO [Block]
getBlocksFromHash db blockHash = undefined

-- kill unperspective forks
prune :: Db -> IO ()
prune db = return () -- TODO: implement me



