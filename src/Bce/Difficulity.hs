module Bce.Difficulity where

import Debug.Trace    
import Bce.Hash    
import Bce.BlockChain
import Bce.BlockChainHash    
import Data.List
import Bce.Util    
import qualified Data.BitString as BS

type LeadingZeros = Int

type SecondsPerBlock = Double

secondsPerBlock :: SecondsPerBlock
secondsPerBlock = 5.0

defaultDifficulity :: LeadingZeros
defaultDifficulity = 1

minDifficulity = defaultDifficulity                     
maxDifficulity = 127                     

difficulityRecalculationBlocks = 3  :: Int

blockDifficulity :: Block -> LeadingZeros
blockDifficulity b =
    length $ takeWhile (==0) $ BS.to01List $ BS.bitString $ hashBs $ hash $ blockHeader b

           
growthSpeed :: BlockChain -> SecondsPerBlock
growthSpeed (BlockChain blocks) =
    let
        recalculationBlocks = take difficulityRecalculationBlocks blocks
        (newest, oldest) = (head recalculationBlocks, last recalculationBlocks)
        blockTimestamp = fromIntegral . bhWallClockTime . blockHeader
        in  (blockTimestamp newest - blockTimestamp oldest) / (fromIntegral difficulityRecalculationBlocks)
    

nextDifficulity :: BlockChain -> LeadingZeros
nextDifficulity bc
    | length (blockChainBlocks bc)  < difficulityRecalculationBlocks = defaultDifficulity
    | otherwise =
        let
            newest = head $ blockChainBlocks bc
            recalculationBlocks = take difficulityRecalculationBlocks $ blockChainBlocks bc 
            avg = blockDifficulity newest
            next = case compare (growthSpeed bc) secondsPerBlock of
                     LT -> avg - 1
                     EQ -> avg
                     GT -> avg + 1
        in min (max next minDifficulity) maxDifficulity
