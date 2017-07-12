module ArbitraryDb where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import qualified Bce.VerifiedDb as VerifiedDb    
import Bce.Hash
import Bce.Crypto    
import Bce.BlockChain
import Bce.BlockChainHash
import Bce.Difficulity    
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
                         , dbFillerNumBlocks :: Int
                         , dbFillerKeys :: Set.Set KeyPair}
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


findOneBlock :: Timer -> Set.Set Transaction -> Difficulity -> BlockId -> IO Block
findOneBlock timer txs target prevBlockId = do
    rnd <- randomIO :: IO Int64
    time <- timer
    case Miner.tryGenerateBlock time rnd prevBlockId txs target of
      Nothing -> findOneBlock timer txs target prevBlockId
      Just b -> return  b

arbitraryPointToBuild :: Db.Db -> Int -> Int -> IO (Difficulity, Block)
arbitraryPointToBuild db maxHeadsNum rnd = do
  heads <- Db.getHeads db
  let Just (arbHeadLength, arbHeadBlock) = randomPick heads rnd
  let blocksBack = if length heads > maxHeadsNum then 1 else arbHeadLength
  Just blocks <- Db.getBlocksTo db (blockId arbHeadBlock) blocksBack
  let Just arbBlock = randomPick blocks rnd 
  Just target <- Db.getNextDifficulityTo db (blockId arbBlock)
  return (target, arbBlock)


instance Arbitrary DbFiller where
    arbitrary = do
      blocksNum <- choose (0, 64) :: Gen Int
      maxHeadsNum <- choose (1, 3) :: Gen Int
      keys <- mapM (\_ -> arbitrary) [1..blocksNum+1]
      let runFiller = (\db -> do
                           (actualBlocksNum, _) <- Db.getLongestHead db
                           if actualBlocksNum > blocksNum
                           then do
                             return()
                           else do
                             rnd <- randomIO :: IO Int
                             (target, arbBlock) <- arbitraryPointToBuild db maxHeadsNum rnd
                             let Just arbKey = randomPick keys rnd 
                             unspent <- Db.unspentAt db (blockId arbBlock)
                             utxs <- Set.fromList <$> generateArbitraryTxs db unspent keys
                             ctx <- Miner.coinbaseTransaction db (keyPairPub arbKey) utxs
                             let txs = Set.insert ctx utxs
                             blk <- findOneBlock now txs target $ blockId arbBlock
                             r <- VerifiedDb.verifyAndPushBlock db blk
                             if not r then error "rejected block"
                             else runFiller db)
      return $ DbFiller runFiller  blocksNum $ Set.fromList keys

testDbPath = "./tmpdb"

flushDb db = do
  Db.unsafeCloseDb db (removeDirectoryRecursive (Db.dbDataDir db))

withDb :: String -> (Db.Db -> IO()) -> IO ()
withDb path = bracket (Db.initDb path) flushDb

data DbPath = DbPath String deriving (Show, Eq)
instance Arbitrary DbPath where
    arbitrary = do
        a <- arbitrary :: Gen Int64
        let p = show $ hash a
        return $ DbPath  p
              
withArbitraryDb :: (Db.Db -> IO()) -> IO ()
withArbitraryDb fx = do
  DbPath arbPath <- generate arbitrary
  withDb arbPath fx
