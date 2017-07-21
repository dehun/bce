module Bce.Verified where

import Data.Ord    
import Bce.BlockChain
import Bce.BlockChainHash    

data VerifiedBlock = VerifiedBlock {verifiedBlock :: Block} deriving (Show, Eq)
data VerifiedTransaction = VerifiedTransaction {verifiedTransaction :: Transaction} deriving (Show, Eq)

instance Ord VerifiedTransaction where
    compare = comparing verifiedTransaction
