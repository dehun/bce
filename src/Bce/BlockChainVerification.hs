module Bce.BlockChainVerification where

import Bce.BlockChain
import Bce.Hash
import Bce.Util    
import Bce.BlockChainHash    
import Data.List
import Data.Ord


    
isCoinbaseTransaction :: Transaction -> Bool    
isCoinbaseTransaction (CoinbaseTransaction _) = True
isCoinbaseTransaction _ = False                                                


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
      coinbasesCorrect = all (\b ->  onlyOne (\t -> isCoinbaseTransaction t) (blockTransactions b)) blocks
      transactionsCorrect = True  -- TODO: check header hash, and every transaction
      difficulitiesCorrect = True -- TODO: recalculate difficulities



