module ArbitraryDb where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import Bce.Hash
import Bce.Crypto    
import Bce.BlockChain
import Bce.BlockChainHash
import Bce.TimeStamp
import Bce.Util    

import GHC.Int(Int64, Int32)        
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

withArbitraryDb :: (Db.Db -> IO()) -> IO ()
withArbitraryDb fx = do
  arbPath <- show <$> hash <$> (randomIO :: IO Int64)
  withDb arbPath fx
