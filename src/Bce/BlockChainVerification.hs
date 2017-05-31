module Bce.BlockChainVerification where

import Bce.BlockChain
import Bce.Hash    
import Bce.BlockChainHash    
import Data.List
import Data.Ord    

verifyBlockChain :: BlockChain -> Bool
verifyBlockChain (BlockChain blocks) =
    and [ prevBlockHashesCorrect
        , timestampsCorrect
        , coinbasesCorrect
        , transactionsCorrect
        , difficulitiesCorrect ]
    where
      blockPairs = zip (init blocks) (tail blocks)
      prevBlockHashesCorrect = all (\(pb, nb) -> hash pb == (bhPrevBlockHeaderHash $ blockHeader pb)) blockPairs
      timestampsCorrect = all (\(pb, nb) -> comparing (bhWallClockTime . blockHeader) pb nb == LT) blockPairs
      coinbasesCorrect = True     -- TODO: every block should contain coinbase, with limited amount of money
      transactionsCorrect = True  -- TODO: check header hash, and every transaction
      difficulitiesCorrect = True -- TODO: recalculate difficulities



