module Bce.Difficulity where

import Debug.Trace    
import Bce.Hash    
import Bce.BlockChain
import Bce.BlockChainHash    
import Data.List
import qualified Data.BitString as BS

type LeadingZeros = Int

secondsPerBlock :: Int
secondsPerBlock = 5

defaultDifficulity :: LeadingZeros
defaultDifficulity = 1

difficulityRecalculationBlocks = 100  :: Int

blockDifficulity :: Block -> LeadingZeros
blockDifficulity b =
    length $ takeWhile (==0) $ BS.to01List $ BS.bitString $ hashBs $ hash $ blockHeader b
    
nextDifficulity :: BlockChain -> LeadingZeros
nextDifficulity (BlockChain blocks)
    | length blocks < difficulityRecalculationBlocks = defaultDifficulity
    | otherwise = 
        let recalculationBlocks = take difficulityRecalculationBlocks blocks
            (lst, fst) = (head recalculationBlocks, last recalculationBlocks)
            blockTimestamp = bhWallClockTime . blockHeader
            timeTookPerBlock = fromIntegral (blockTimestamp fst - blockTimestamp lst) / (fromIntegral difficulityRecalculationBlocks) :: Double
        in case compare timeTookPerBlock $ fromIntegral secondsPerBlock of
             LT -> blockDifficulity lst + 1 
             EQ -> blockDifficulity lst
             GT -> blockDifficulity lst - 1
