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
spec = parallel $ do
  around withArbitraryDb $ do
    describe "Verification" $ do
      it "passes normal miner blocks" $ \db -> property $ \filler keyPair -> do
         (dbFillerRun filler) db
         blk <- Miner.findBlock db (keyPairPub keyPair) now
         r <- runEitherT $ Verification.verifyBlock db blk
         r `shouldSatisfy` isRight
      it "passes findOneBlock" $ \db -> property $ \filler keyPair rnd -> do
         (dbFillerRun filler) db
         Just blocksFrom <- Db.getBlocksFrom db (blockId initialBlock)
         let idx = (abs rnd) `mod` length blocksFrom
--         putStrLn $ "blocks are" ++ show (map (show . hash) blocksFrom)
         let block = if blocksFrom == [] then initialBlock else blocksFrom !! idx
         Just target <- Db.getNextDifficulityTo db (blockId block)
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
      it "catches wrong transactions hash mismatch" $ \db -> property $ \filler keyPair -> do
         (dbFillerRun filler) db
         target <- Db.getNextDifficulity db
         cbtx <- Miner.coinbaseTransaction db (keyPairPub keyPair) Set.empty
         b <- findOneBlock now (Set.singleton cbtx) target unexistingBlockId
         (_, topBlock) <- Db.getLongestHead db                                  
         unspent <- Db.unspentAt db (blockId topBlock)
         Just (ntx, _) <- generateArbitraryTx db unspent (Set.toList $ dbFillerKeys filler)
         let nb = b {blockTransactions = Set.insert ntx $ blockTransactions b}
         r <- runEitherT $ Verification.verifyBlock db nb
         r `shouldSatisfy` isLeft                   
                                 
