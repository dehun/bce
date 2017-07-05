module DbFsSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import Bce.Miner
import Bce.Hash
import Bce.BlockChain
import Bce.BlockChainHash

import Test.Hspec
import System.Directory
import Control.Exception    
import qualified Data.Set as Set    
    


testDbPath = "./tmpdb"

flushDb db = do
  Db.unsafeCloseDb db
  removeDirectoryRecursive (Db.dbDataDir db)

withDb :: String -> (Db.Db -> IO()) -> IO ()
withDb path = bracket (Db.initDb path) flushDb
  

spec :: Spec
spec = do
  around (withDb testDbPath) $ do
         describe "DbFs blockchain" $ do
           it "empty database have only initial block" $ \db -> do
                   Db.getLongestHead db `shouldReturn` (1, initialBlock)
           it "empty database getBlocksFrom " $ \db -> do
                   Db.getBlocksFrom db (blockId initialBlock) `shouldReturn` Just []
           it "empty database getBlocksFrom unknonw block id " $ \db -> do
                   Db.getBlocksFrom db (hash "asd") `shouldReturn` Nothing
           it "empty database getBlocksTo unknonw block id " $ \db -> do
                   Db.getBlocksTo db (blockId initialBlock) 1 `shouldReturn` []
           it "empty database have only one unspent" $ \db -> do
                   Db.unspentAt db (blockId initialBlock)
                         `shouldReturn` Set.singleton (TxOutputRef (transactionId initialBlock (head $ Set.toList $ blockTransactions initialBlock)) 0)
           it "empty database get initial block" $ \db -> do
                   Db.getBlock db (blockId initialBlock) `shouldReturn` Just initialBlock
           it "empty database get unknown block" $ \db -> do
                   Db.getBlock db (hash "what") `shouldReturn` Nothing                     
