module DbFsSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import Bce.Hash
import Bce.Crypto    
import Bce.BlockChain
import Bce.BlockChainHash
import Bce.TimeStamp    

import Test.Hspec
import Test.QuickCheck    
import Test.QuickCheck.Arbitrary    
import System.Directory
import Control.Exception    
import qualified Data.Set as Set
import qualified Data.ByteString as BS        
    

data DbFiller = DbFiller { dbFillerRun :: Db.Db -> IO ()
                         , dbFillerNumBlocks :: Int}
instance Show DbFiller where
    show f = "DbFiller with N=" ++ show (dbFillerNumBlocks f)
    
instance Arbitrary DbFiller where
    arbitrary = do
      blocksNum <- choose (0, 64) :: Gen Int
      let ownerKey = PubKey $ BS.pack [0xde, 0xad]
      return $ DbFiller (\db -> mapM_ (\_ -> Miner.growOneBlock db ownerKey now)  [1..blocksNum]) blocksNum

testDbPath = "./tmpdb"

flushDb db = do
  Db.unsafeCloseDb db
  removeDirectoryRecursive (Db.dbDataDir db)

withDb :: String -> (Db.Db -> IO()) -> IO ()
withDb path = bracket (Db.initDb path) flushDb

unexistingBlockId = hash "does not exist"

spec :: Spec
spec = do
  around (withDb testDbPath) $ do
         describe "DbFs blockchain" $ do
           it "empty database have only initial block" $ \db -> do
                   Db.getLongestHead db `shouldReturn` (1, initialBlock)
           it "empty database getBlocksFrom " $ \db -> do
                   Db.getBlocksFrom db (blockId initialBlock) `shouldReturn` Just []
           it "empty database have only one unspent" $ \db -> do
                   Db.unspentAt db (blockId initialBlock)
                         `shouldReturn` Set.singleton (TxOutputRef (transactionId initialBlock (head $ Set.toList $ blockTransactions initialBlock)) 0)
           it "empty database have no transactions to mine" $ \db -> do
                   Db.getTransactions db `shouldReturn` Set.empty
           it "getBlocksFrom unknown block id " $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.getBlocksFrom db (hash initialBlock) `shouldReturn` Nothing
           it "getBlocksTo unknonw block id " $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.getBlocksTo db unexistingBlockId 1 `shouldReturn` []
           it "get initial block" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db                                             
                   Db.getBlock db (blockId initialBlock) `shouldReturn` Just initialBlock
           it "longest head is correct" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db                                                   
                   fst <$> (Db.getLongestHead db)  `shouldReturn` (1 + dbFillerNumBlocks filler)
           it "database get unknown block" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.getBlock db unexistingBlockId `shouldReturn` Nothing
           it "unknown block does not exists" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.isBlockExists db unexistingBlockId `shouldReturn` False
           it "initial block does exists" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.isBlockExists db (blockId initialBlock) `shouldReturn` True
           it "head block does exists" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, block) <- Db.getLongestHead db
                   Db.isBlockExists db (blockId block) `shouldReturn` True
