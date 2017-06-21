module Bce.BlockChainVerification where

import Bce.BlockChain
import Bce.Hash
import Bce.Util    
import Bce.BlockChainHash
import Bce.Difficulity    
import Data.List
import Data.Ord
import Debug.Trace    


    
isCoinbaseTransaction :: Transaction -> Bool    
isCoinbaseTransaction (CoinbaseTransaction _) = True
isCoinbaseTransaction _ = False                                                

blockTimestamp = bhWallClockTime . blockHeader

blocksForTimeAveraging = 10                 
                          
verifyBlockChain :: BlockChain -> Bool
verifyBlockChain (BlockChain blocks) =
    and [ prevBlockHashesCorrect
        , timestampsCorrect
        , coinbasesCorrect
        , transactionsCorrect
        , difficulitiesCorrect
        ]
    where
      blockPairs = zip (init blocks) (tail blocks)
      prevBlockHashesCorrect =
          all (\(nb, pb) -> hash (blockHeader pb) == (bhPrevBlockHeaderHash $ blockHeader nb)) blockPairs
      timestampsCorrect =
          let groups = init $ init $ map (take blocksForTimeAveraging) (tails blocks)
              predicate h r = blockTimestamp h > median (map blockTimestamp r)
          in all (\(h:t) -> predicate h t) groups
      coinbasesCorrect =
          all (\b ->  onlyOne (\t -> isCoinbaseTransaction t) (blockTransactions b)) blocks -- TODO: reward should be checked
      transactionsCorrect = all (\b -> and [ transactionsHashCorrect b 
                                           , transactionsReferencesCorrect b]) blocks
      transactionsHashCorrect block = bhTransactionsHash (blockHeader block) == hash (blockTransactions block)
      transactionsReferencesCorrect block = True -- TODO: check                                     
      difficulitiesCorrect =
          let ts = init $ zip blocks (tail $ tails blocks)
              difficulitiesStampedCorrectly =
                  all (\(b, t) -> bhDifficulity (blockHeader b)
                                  == fromIntegral (nextDifficulity $ t)) ts
              difficulitiesOfBlocksMatchBlocks =
                  all (\b -> blockDifficulity b
                             >= fromIntegral (bhDifficulity $ blockHeader b))
                          blocks
          in and [ difficulitiesOfBlocksMatchBlocks
                 , difficulitiesStampedCorrectly
                 ]
