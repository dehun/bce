module DbFsSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import Bce.Hash
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
    

data DbFiller = DbFiller { dbFillerRun :: Db.Db -> IO ()
                         , dbFillerNumBlocks :: Int}
instance Show DbFiller where
    show f = "DbFiller with N=" ++ show (dbFillerNumBlocks f)

instance Arbitrary PubKey where
    arbitrary = PubKey <$> fastRandBs 16
instance Arbitrary PrivKey where
    arbitrary = PrivKey <$> fastRandBs 16

instance Arbitrary KeyPair where
    arbitrary = KeyPair <$> arbitrary <*> arbitrary

generateArbitraryTx :: Db.Db -> Set.Set TxOutputRef -> [KeyPair] -> IO (Maybe (Transaction, Set.Set TxOutputRef))
generateArbitraryTx db unspent keys
    | or [unspent == Set.empty, length keys == 0] = return Nothing
    | otherwise = do
  outputIdx <- mod <$> randomIO <*> pure (Set.size unspent)
  let outputRef = (Set.toList unspent) !! outputIdx
  Just output <- Db.resolveInputOutput db (TxInput outputRef)
  toArbIdx <- mod <$> randomIO <*> pure (length keys)
  let toArbKey = keys !! toArbIdx
  let newTxInputs = Set.singleton $ TxInput outputRef
  let newTxOutputs = Set.singleton $ TxOutput (outputAmount output) (keyPairPub toArbKey)
  let Just outputKeyPair = find (\ks -> keyPairPub ks == outputPubKey output) keys
  let newTxSign = sign (hash (newTxInputs, newTxOutputs)) (keyPairPriv outputKeyPair)
  let newTx = Transaction newTxInputs newTxOutputs newTxSign
  return $ Just (newTx, Set.singleton outputRef)

                
generateArbitraryTxs :: Db.Db -> Set.Set TxOutputRef -> [KeyPair] -> IO [Transaction]
generateArbitraryTxs db unspent' keys = do
  numArbTxs <- mod <$> randomIO <*> pure (Set.size unspent')
  (_, txs) <- foldM (\(unspent, txs) _ -> do
                       r <- generateArbitraryTx db unspent keys
                       case r of
                         Just (tx, spent) -> return (Set.difference unspent spent, tx:txs)
                         Nothing -> return (unspent, txs)) (unspent', []) [1..numArbTxs]
  return txs
    
instance Arbitrary DbFiller where
    arbitrary = do
      blocksNum <- choose (0, 32) :: Gen Int
      keys <- mapM (\_ -> arbitrary) [1..blocksNum]
      return $ DbFiller (\db -> mapM_ (\k -> do
                                         (_, topBlock) <- Db.getLongestHead db
                                         unspent <- Db.unspentAt db (blockId topBlock)
                                         txs <- generateArbitraryTxs db unspent keys
                                         Db.pushTransactions db (Set.fromList txs)
                                         Miner.growOneBlock db (keyPairPub k) now
                                      )  keys) blocksNum

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
                   Db.getBlocksFrom db (hash unexistingBlockId) `shouldReturn` Nothing
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
           it "resolveInputOutput on empty db" $ \db -> do
                   let initTx = head $ Set.toList $ blockTransactions $ initialBlock
                   o <- Db.resolveInputOutput db (TxInput (TxOutputRef (transactionId initialBlock initTx) 0))
                   o `shouldSatisfy` isJust
           it "all unspents are there" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, topBlock) <- Db.getLongestHead db
                   unspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   mapM_ (\u -> do
                           r <- Db.resolveInputOutput db (TxInput u)
                           r `shouldSatisfy` isJust) unspent
           it "does not loose money" $ \db -> property $ \filler -> do
                   (dbFillerRun filler) db
                   (_, topBlock) <- Db.getLongestHead db
                   unspent <- Set.toList <$> Db.unspentAt db (blockId topBlock)
                   resolvesOpts <- mapM (\u -> Db.resolveInputOutput db (TxInput u)) unspent
                   let Just resolves = sequence resolvesOpts
                   let totalCoins = sum (map outputAmount resolves)
                   totalCoins `shouldBe`
                              fromIntegral (Db.baseCoinbaseReward * (fromIntegral $ 1 + dbFillerNumBlocks filler))
                   
