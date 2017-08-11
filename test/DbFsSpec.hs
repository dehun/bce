module DbFsSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import qualified Bce.VerifiedDb as VerifiedDb            
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
import Control.Monad.Extra    
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
           it "all keys balances = unspent" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   unspent <- Db.unspentAt db (blockId topBlock)
                   keysUnspents <- concatMapM (\k -> Set.toList <$> fst <$> Db.getPubKeyBalance db (keyPairPub k))
                                   $ Set.toList (Set.insert initialBlockKeyPair $ dbFillerKeys filler)
                   Set.fromList keysUnspents `shouldBe` unspent                                     
           it "correctly consumes in memory transactions when built on largest head"
                  $ \db -> property $ \filler arbKeyPair -> do
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   unspent <- Db.unspentAt db (blockId topBlock)
                   let keys = initialBlockKeyPair : (Set.toList $ dbFillerKeys filler)
                   txsInMem <- generateArbitraryTxs db unspent keys
                   let inputsInMem = Set.fromList $ concatMap (Set.toList . allTxInputs) txsInMem
                   Right() <- VerifiedDb.verifyAndPushTransactions db $ Set.fromList txsInMem
                   txsInBlock <- generateArbitraryTxs db unspent keys
                   let inputsInBlock = Set.fromList $ concatMap (Set.toList . allTxInputs) txsInBlock
                   let txsToBeConsumed = filter (\tx -> Set.empty /= Set.intersection inputsInBlock (allTxInputs tx)) txsInMem
                   -- generate block
                   Just target <- Db.getNextDifficulityTo db (blockId  topBlock)
                   ctx <- Miner.coinbaseTransaction db (keyPairPub arbKeyPair) $ Set.fromList txsInBlock
                   blk <- findOneBlock now (Set.insert ctx $ Set.fromList txsInBlock) target $ blockId topBlock
                   VerifiedDb.verifyAndPushBlock db blk

                   txsInMemAfter <- Db.getTransactions db
                   txsInMemAfter `shouldBe` Set.difference (Set.fromList txsInMem) (Set.fromList txsToBeConsumed)
           it "correctly gets unconfirmed for key" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   let keys = initialBlockKeyPair : (Set.toList $ dbFillerKeys filler)
                   keysWithBalance <- filterM (\k -> do
                                                     (u, _) <- Db.getPubKeyBalance db $ keyPairPub k
                                                     return $ (length u) > 0) keys
                   toArbIdx <- mod <$> randomIO <*> pure (length keysWithBalance)
                   let toArbKey = keysWithBalance !! toArbIdx
                   (unspentBefore, unconfirmedBefore) <- Db.getPubKeyBalance db $ keyPairPub toArbKey
                   unconfirmedBefore `shouldBe` Set.empty
                   txs <- generateArbitraryTxs db unspentBefore [toArbKey]
                   Right () <- VerifiedDb.verifyAndPushTransactions db $ Set.fromList txs
                   (unspentAfter, unconfirmedAfter) <- Db.getPubKeyBalance db $ keyPairPub toArbKey
                   unspentAfter `shouldBe` unspentBefore
                   unconfirmedAfter `shouldBe`
                                        (Set.fromList
                                                $ concatMap (\tx -> map inputOutputRef
                                                                    $ Set.toList $ allTxInputs tx) txs)
           it "does not loose money" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   unspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   resolvesOpts <- mapM (\u -> Db.resolveInputOutput db (TxInput u)) unspent
                   let Just resolves = sequence resolvesOpts
                   let totalCoins = sum (map outputAmount resolves)
                   totalCoins `shouldBe`
                              fromIntegral (Db.baseCoinbaseReward * (fromIntegral $ 1 + dbFillerNumBlocks filler))
           it "loads properly" $ \db -> property $ \filler -> do
--                   pendingWith "do proper copy "
                   (dbFillerRun filler) db
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead db
                   oldUnspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   -- kill old one
                   Db.unsafeCloseDb db (return ())
--                   flushDb db                                 
                   -- load new one
                   newDb <- Db.initDb (Db.dbDataDir db)
                   Db.loadDb newDb
                   fst <$> (Db.getLongestHead newDb)  `shouldReturn` (1 + dbFillerNumBlocks filler)
                   (_, VerifiedBlock topBlock) <- Db.getLongestHead newDb
                   newUnspent <- Set.toList <$> Db.unspentAt newDb (blockId topBlock)                       
                   oldUnspent `shouldBe` newUnspent
                   flushDb db
                   flushDb newDb
