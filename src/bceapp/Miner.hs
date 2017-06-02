import Bce.Crypto
import Bce.Hash
import Bce.BlockChain
import Bce.BlockChainHash    
import Bce.BlockChainVerification
import Bce.InitialBlock
import Bce.Difficulity    

import Debug.Trace    

import GHC.Int (Int64)

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Control.Monad    


randomBlock :: Int -> Int -> Block -> Difficulity -> Block
randomBlock time rnd prevBlock difficulity =
    let transactions = [CoinbaseTransaction [TxOutput 0 ""]]
        header = BlockHeader
                 (hash transactions)
                 (hash $ blockHeader prevBlock)
                 (fromIntegral rnd)
                 (fromIntegral time)
                 (fromIntegral difficulity)
    in Block header transactions

    
growChain :: Int -> BlockChain -> IO BlockChain
growChain rnd oldchain = do
  let (BlockChain blocks) = oldchain
  nextTime <- fmap round getPOSIXTime
  let nextblock = randomBlock nextTime rnd (head blocks) (nextDifficulity oldchain)
  let newChain = BlockChain (nextblock : blocks)
  if verifyBlockChain newChain
  then do
    putStrLn $ show nextTime ++ " got chain of length " ++ show (length (blockChainBlocks newChain))
                 ++ "; used rnd"
                 ++ "; block difficulity is " ++ (show (blockDifficulity (head $ blockChainBlocks newChain)))
                 ++ "; next difficulity is " ++ (show $ nextDifficulity newChain)
                 ++ "; growth speed is " ++ (show $ growthSpeed newChain)
    return newChain
  else growChain (rnd + 1) (BlockChain blocks)


main = do
  let initialChain = BlockChain [initialBlock]
  growed <- foldM (\acc i -> growChain i acc) initialChain [1..]
  putStrLn $ show growed
