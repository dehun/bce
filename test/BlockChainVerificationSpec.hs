{-# LANGUAGE BangPatterns #-}

module BlockChainVerificationSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import Bce.Hash
import Bce.Crypto    
import Bce.BlockChain
import Bce.Difficulity    
import Bce.BlockChainHash
import Bce.TimeStamp
import Bce.Util

import ArbitraryDb    

import Debug.Trace    
import Test.Hspec
import Test.QuickCheck    
import Test.QuickCheck.Arbitrary    
import System.Directory
import Control.Exception
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either    
import Data.List
import GHC.Int(Int64, Int32)    
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Data.ByteString.Arbitrary
import Control.Monad
import System.Random

import qualified Bce.BlockChainVerification as Verification

unexistingBlockId = hash "does not exist"

spec :: Spec
spec =  do
  around withArbitraryDb $ do
    describe "Verification" $ parallel $ do
      it "passes normal miner blocks" $ \db -> property $ \filler keyPair -> do
         (dbFillerRun filler) db
         blk <- Miner.findBlock db (keyPairPub keyPair) now
         r <- runEitherT $ Verification.verifyBlock db blk
         r `shouldSatisfy` isRight
      it "passes findOneBlock" $ \db -> property $ \filler keyPair rnd maxHeads -> do
         (dbFillerRun filler) db
         (target, block) <- arbitraryPointToBuild db maxHeads rnd
         cbtx <- Miner.coinbaseTransaction db (keyPairPub keyPair) Set.empty
         b <- findOneBlock now (Set.singleton cbtx) target (blockId block)
         r <- runEitherT $ Verification.verifyBlock db b
         r `shouldSatisfy` isRight              
      it "catches unexisting prev block" $ \db -> property $ \filler keyPair -> do
         (dbFillerRun filler) db
         target <- Db.getNextDifficulity db
         cbtx <- Miner.coinbaseTransaction db (keyPairPub keyPair) Set.empty
         b <- findOneBlock now (Set.singleton cbtx) target unexistingBlockId
         r <- runEitherT $ Verification.verifyBlock db b
         r `shouldSatisfy` isLeft
      it "catches wrong transactions hash mismatch" $ \db -> property $ \filler keyPair maxHeads rnd -> do
         (dbFillerRun filler) db
         (target, block) <- arbitraryPointToBuild db maxHeads rnd
         cbtx <- Miner.coinbaseTransaction db (keyPairPub keyPair) Set.empty
         b <- findOneBlock now (Set.singleton cbtx) target (blockId block)
         unspent <- Db.unspentAt db (blockId block)
         Just (ntx, _) <- generateArbitraryTx db unspent (Set.toList $ dbFillerKeys filler)
         let nb = b {blockTransactions = Set.insert ntx $ blockTransactions b}
         r <- runEitherT $ Verification.verifyBlock db nb
         r `shouldSatisfy` isLeft
         r `shouldSatisfy` (\(Left m) -> "wrong stamped transactions hash" `isInfixOf` m)
      it "catches out of time block" $ \db -> property $ \filler keyPair rnd maxHeads -> do
         (dbFillerRun filler) db
         (target, block) <- arbitraryPointToBuild db maxHeads rnd 
         cbtx <- Miner.coinbaseTransaction db (keyPairPub keyPair) Set.empty                              
         t <- now 
         b <- findOneBlock (return $ t - 10^9) (Set.singleton cbtx) target (blockId block)
         r <- runEitherT $ Verification.verifyBlock db b
         r `shouldSatisfy` isLeft
         r `shouldSatisfy` (\(Left m) -> "block timestamp is incorrect" `isInfixOf` m)           
      it "catches multiple coinbase transactions with different keys"
             $ \db -> property $ \filler keyPair1 keyPair2 rnd maxHeads -> do
         (dbFillerRun filler) db
         (target, block) <- arbitraryPointToBuild db maxHeads rnd 
         cbtx1 <- Miner.coinbaseTransaction db (keyPairPub keyPair1) Set.empty
         cbtx2 <- Miner.coinbaseTransaction db (keyPairPub keyPair2) Set.empty
         b <- findOneBlock now (Set.fromList [cbtx1, cbtx2]) target (blockId block)
         r <- runEitherT $ Verification.verifyBlock db b
         r `shouldSatisfy` isLeft
         r `shouldSatisfy` (\(Left m) -> "more than one coinbase per block" `isInfixOf` m)
