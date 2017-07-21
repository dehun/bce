module DbFsSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import Bce.Hash
import Bce.Verified    
import Bce.Crypto    
import Bce.BlockChain
import Bce.BlockChainHash
import Bce.TimeStamp
import Bce.Util    

import Test.Hspec
import Test.QuickCheck    
import Test.QuickCheck.Arbitrary    
import System.Directory
import Control.Exception
import Data.Maybe
import Data.List    
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Data.ByteString.Arbitrary
import Control.Monad
import System.Random
import ArbitraryDb    
    

unexistingBlockId = hash "does not exist"


spec :: Spec
spec = do
  around withArbitraryDb $ do
         describe "DbFs database" $ parallel $ do
           it "empty database have only initial block" $ \db -> do
                   Db.getLongestHead db `shouldReturn` (1, verifiedInitialBlock)
           it "empty database getBlocksFrom " $ \db -> do
                   Db.getBlocksFrom db (blockId initialBlock) `shouldReturn` Just []
           it "empty database have only one unspent" $ \db -> do
                   Db.unspentAt db (blockId initialBlock)
                         `shouldReturn` Set.singleton (TxOutputRef (transactionId initialBlock (head $ Set.toList $ blockTransactions initialBlock)) 0)
           it "empty database have no transactions to mine" $ \db -> do
                   Db.getTransactions db `shouldReturn` Set.empty
           it "empty database getBlocksFrom unknown block id " $ \db -> do
                   Db.getBlocksFrom db (hash unexistingBlockId) `shouldReturn` Nothing
           it "empty database getBlocksTo initial block id " $ \db -> 
                   Db.getBlocksTo db (blockId initialBlock) 1 `shouldReturn` Just [verifiedInitialBlock]
           it "getBlocksFrom unknown block id " $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.getBlocksFrom db (hash unexistingBlockId) `shouldReturn` Nothing
           it "getBlocksFrom initial block id " $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Just blks <- Db.getBlocksFrom db (blockId initialBlock)
                   length blks `shouldBe` dbFillerNumBlocks filler
           it "getBlocksTo unknonw block id " $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.getBlocksTo db unexistingBlockId 1 `shouldReturn` Nothing
           it "getBlocksTo initial block id return initialBlock" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   Db.getBlocksTo db (blockId initialBlock) 1 `shouldReturn` Just [verifiedInitialBlock]
           it "getBlocksTo longest head - length is correct" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (longest, VerifiedBlock  headBlock) <- Db.getLongestHead db
                   bs <- Db.getBlocksTo db (blockId headBlock) (2*longest)
                   length <$> bs `shouldBe` Just longest
           it "getBlocksTo longest head - last is initial" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (longest, VerifiedBlock headBlock) <- Db.getLongestHead db
                   bs <- Db.getBlocksTo db (blockId headBlock) longest                                           
                   last <$> bs `shouldBe` Just verifiedInitialBlock
           it "getBlocksTo longest head - head is head" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (longest, VerifiedBlock headBlock) <- Db.getLongestHead db
                   bs <- Db.getBlocksTo db (blockId headBlock) longest
                   head <$> bs `shouldBe` Just (VerifiedBlock headBlock)
           it "get initial block" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db                                             
                   Db.getBlock db (blockId initialBlock) `shouldReturn` Just verifiedInitialBlock
           it "longest head is correct" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db                                                   
                   fst <$> (Db.getLongestHead db) `shouldReturn` (1 + dbFillerNumBlocks filler)
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
                   (_, VerifiedBlock block) <- Db.getLongestHead db
                   Db.isBlockExists db (blockId block) `shouldReturn` True
           it "resolveInputOutput on empty db" $ \db -> do
                   let initTx = head $ Set.toList $ blockTransactions $ initialBlock
                   o <- Db.resolveInputOutput db (TxInput (TxOutputRef (transactionId initialBlock initTx) 0))
                   o `shouldSatisfy` isJust
           it "all unspents are there" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   unspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   mapM_ (\u -> do
                           r <- Db.resolveInputOutput db (TxInput u)
                           r `shouldSatisfy` isJust) unspent
           it "does not loose money" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   unspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   resolvesOpts <- mapM (\u -> Db.resolveInputOutput db (TxInput u)) unspent
                   let Just resolves = sequence resolvesOpts
                   let totalCoins = sum (map outputAmount resolves)
                   totalCoins `shouldBe`
                              fromIntegral (Db.baseCoinbaseReward * (fromIntegral $ 1 + dbFillerNumBlocks filler))
           it "loads properly" $ \db -> property $ \filler (DbPath arbDbPath) -> do
                   pendingWith "do proper copy "
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   oldUnspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   -- prepare new dir
                   createDirectoryIfMissing False arbDbPath
                   toCopy <- filter (\p -> or [ p == "transactions.db"
                                              , p == "blocks.db"
                                              , dropWhile (/='.') p == ".blk"]) <$> listDirectory (Db.dbDataDir db)
                   mapM_ (\f -> copyFile ((Db.dbDataDir db) ++ "/" ++ f) ((arbDbPath ++ "/" ++ f))) toCopy
                   -- kill old one
                   flushDb db
                   
                   newDb <- Db.initDb arbDbPath
                   Db.loadDb newDb
                   fst <$> (Db.getLongestHead newDb)  `shouldReturn` (1 + dbFillerNumBlocks filler)
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead newDb
                   newUnspent <- Set.toList <$> Db.unspentAt newDb (blockId topBlock)                       
                   oldUnspent `shouldBe` newUnspent
                   flushDb newDb
                   

                                        
                   
