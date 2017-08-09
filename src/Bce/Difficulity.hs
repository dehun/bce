module Bce.Difficulity where

import Debug.Trace    
import Bce.Hash    
import Bce.BlockChain
import Bce.BlockChainHash    
import Data.List
import Bce.Util    

type LeadingZeros = Int
type Difficulity = LeadingZeros    

type SecondsPerBlock = Double

secondsPerBlock :: SecondsPerBlock
secondsPerBlock = 1.0

defaultDifficulity :: Difficulity
defaultDifficulity = 1

minDifficulity = defaultDifficulity                     
maxDifficulity = 127

difficulityRecalculationBlocks = 25  :: Int


blockDifficulity :: Block -> Difficulity
blockDifficulity b =
    length $ takeWhile (==0) $ to01List $ hashBs $ hash $ blockHeader b

           
growthSpeed :: [Block] -> SecondsPerBlock
growthSpeed blocks =
    let
        recalculationBlocks = take difficulityRecalculationBlocks blocks
        blockTimestamp = fromIntegral . bhWallClockTime . blockHeader                              
        blockTimesDiff (l, r) = abs (blockTimestamp l - blockTimestamp r)
        times = map blockTimesDiff $ zipPairs recalculationBlocks
    in average times


comparsionThresholdSeconds = secondsPerBlock / 10
    
nextDifficulity :: [Block] -> LeadingZeros
nextDifficulity blocks
    | length blocks  < difficulityRecalculationBlocks = defaultDifficulity
    | otherwise =
        let
            newest = head $ blocks
            recalculationBlocks = take difficulityRecalculationBlocks $ blocks
            lastDifficulity = fromIntegral $ bhDifficulity $ blockHeader newest
            avgDifficulity = round $ average $ map (fromIntegral .bhDifficulity . blockHeader) recalculationBlocks
            next = case compare (growthSpeed blocks) secondsPerBlock of
                     LT -> avgDifficulity + 1
                     EQ -> avgDifficulity
                     GT -> avgDifficulity - 1
        in if abs (growthSpeed blocks - secondsPerBlock) < comparsionThresholdSeconds
           then lastDifficulity
           else min (max next minDifficulity) maxDifficulity
