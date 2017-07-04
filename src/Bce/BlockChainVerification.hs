module Bce.BlockChainVerification
    ( verifyBlock
    , verifyTransaction
    , verifyTransactionTransaction)
    where

import Bce.BlockChain
import Bce.InitialBlock    
import Bce.Hash
import Bce.BlockChainHash
import Bce.BlockChainSerialization    
import Bce.TimeStamp
import Bce.Difficulity    
import Bce.Util
import Bce.Logger
import Bce.Crypto
import qualified Bce.DbFs as Db    

import Data.Maybe    
import Data.Either
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import qualified Data.Set as Set    


verifyPrevBlockHashCorrect db block
    | block == initialBlock = return ()
    | otherwise = do
  let prevHash = bhPrevBlockHeaderHash $ blockHeader block
  exists <- liftIO $ Db.isBlockExists db prevHash
  guard exists `mplus` left "wrong prev block hash"


verifyBlockDifficulity db block = do
  let prevHash = (bhPrevBlockHeaderHash $ blockHeader block)
  prevBlocks <- liftIO $ Db.getBlocksTo db prevHash difficulityRecalculationBlocks
  let expectedDifficulity = nextDifficulity prevBlocks
  let actualDifficulity = blockDifficulity block
  let stampedDifficulity = fromIntegral $ bhDifficulity $ blockHeader block
  guard (stampedDifficulity == expectedDifficulity) `mplus` left "wrong stamped difficulity"
  guard (actualDifficulity >= expectedDifficulity) `mplus` left "wrong difficulity"


blocksForTimeAveraging = 10                         
verifyBlockTimestamp db block = do
  let blockTimestamp  = bhWallClockTime . blockHeader
  lastBlocks <- liftIO $ Db.getBlocksTo db (hash block) blocksForTimeAveraging
  case lastBlocks of
    [] -> return ()
    _ -> do
      let avgTime = median $ map blockTimestamp lastBlocks
      guard (blockTimestamp block >= avgTime) `mplus` left "block timestamp is incorrect, less than last avg"


verifyTransactionSignature db tx =
    case tx of
      Transaction inputs outputs sig -> do
              inputOutputsOptSeq <- liftIO $ mapM (Db.resolveInputOutput db) (Set.toList inputs)
              let inputOutputsOpt = sequence inputOutputsOptSeq
              guard (isJust inputOutputsOpt)
                        `mplus` left "can not resolve input's output for transaction"
              let inputOutputs = fromJust inputOutputsOpt
              let pubKey = outputPubKey $ head inputOutputs
              guard (all (\o -> pubKey == outputPubKey o) inputOutputs)
                        `mplus` left "all input pub keys should match"
              guard (verifySignature sig pubKey (hash tx)) `mplus` left "transaction signature is incorrect"

verifyTransactionTransaction db tx@(Transaction inputs outputs sig) = do
  fee <- liftIO $ Db.transactionFee db tx
  guard (isJust fee) `mplus` left "transaction fee can not be calculated (absent input?)"
  guard (fromJust fee >= 0) `mplus` left "transaction fee is below zero"
  guard (length inputs > 0) `mplus` left "there are should be at least one transaction input"
  guard (length outputs > 0) `mplus` left "there are should be at least one transaction output"
  verifyTransactionSignature db tx
verifyTransactionTransaction db _ = left "coinbase transaction is not allowed"


verifyTransaction db block tx = 
  case tx of
    CoinbaseTransaction outputs -> do
            guard (onlyOne isCoinbaseTransaction $ Set.toList $ blockTransactions block)
                      `mplus` left "more than one coinbase per block"
            guard (1 == length outputs)
                      `mplus` left "more than one output in coinbase transaction"
            expectedCoinbaseReward <- liftIO $ Db.maxCoinbaseReward db (Set.toList $ blockTransactions block)
            guard (isJust expectedCoinbaseReward) `mplus` left "can not calculate or wrong coinbase reward"
            guard ((outputAmount $ head $ Set.toList $ outputs) == fromJust expectedCoinbaseReward)
                      `mplus` left "coinbase reward is incorrectly stamped"
    Transaction inputs outputs sig -> verifyTransactionTransaction db tx


verifyBlockTransactions db block = do
  let txs = blockTransactions block
  guard (hash txs == bhTransactionsHash (blockHeader block)) `mplus` left "wrong stamped transactions hash"
  let allInputs = concatMap (\tx -> case tx of
                                          CoinbaseTransaction _ -> []
                                          _ -> Set.toList $ txInputs tx
                            ) $ Set.toList txs
  guard (all (\inp -> onlyOne (==inp) allInputs) allInputs) `mplus` left "input used more than once"
  unspent <- liftIO $ Db.unspentAt db (bhPrevBlockHeaderHash $ blockHeader block)
  let spentOutputs = Set.fromList $ map inputOutputRef allInputs
  guard (Set.isSubsetOf spentOutputs unspent) `mplus` left "double spend attempt"
  mapM_ (\tx -> verifyTransaction db block tx
                `mplus` (left $ "; in transaction" ++ show (transactionId block tx))) txs


verifyBlockDoesNoDoHashCollision db block = do
  alreadyExists <- liftIO $  Db.isBlockExists db (blockId block)
  guard (alreadyExists) `mplus` left "block already exists, or block id collides"


verifyBlock :: Db.Db -> Block -> EitherT String IO [()]
verifyBlock db block
    | block == initialBlock = return [()] -- initial block is valid, even though it does not have prev block
    | otherwise = do
    sequence [ verifyPrevBlockHashCorrect db block
             , verifyBlockDifficulity db block
             , verifyBlockTimestamp db block
             , verifyBlockTransactions db block] `mplus` (left $ "; in block" ++ (show $ hash block))
    
