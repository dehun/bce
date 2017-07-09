{-# LANGUAGE BangPatterns #-}

module BlockChainVerificationSpec where

import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import qualified Bce.Miner as Miner
import Bce.Hash
import Bce.Crypto    
import Bce.BlockChain
import Bce.BlockChainHash
import Bce.TimeStamp
import Bce.Util    

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
import ArbitraryDb

import qualified Bce.BlockChainVerification as Verification

unexistingBlockId = hash "does not exist"

findOneBlock timer txs target prevBlockId = do
    putStrLn $ "searching block with target=" ++ show target
    rnd <- randomIO :: IO Int64
    time <- timer
    case Miner.tryGenerateBlock time rnd prevBlockId txs target of
      Nothing -> findOneBlock timer txs target prevBlockId
      Just b -> return b
                    

spec :: Spec
spec = do
  around withArbitraryDb $ do
    describe "Verification" $ do
      it "passes normal miner blocks" $ \db -> property $ \filler keyPair -> do
         (dbFillerRun filler) db                                                 
         blk <- Miner.findBlock db (keyPairPub keyPair) now
         r <- runEitherT $ Verification.verifyBlock db blk
         r `shouldSatisfy` isRight
      it "passes findOneBlock" $ \db -> property $ \filler keyPair rnd -> do
         pendingWith "werid rance condition!"
         (dbFillerRun filler) db
         Just blocksFrom <- Db.getBlocksFrom db (blockId initialBlock)
         let idx = (abs rnd) `mod` length blocksFrom
         putStrLn $ "blocks are" ++ show (map (show . hash) blocksFrom)
         let block = if blocksFrom == [] then initialBlock else blocksFrom !! idx
         target <- Db.getNextDifficulityTo db (blockId block)
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
                                 
