module Bce.BlockChainSerialization where

import Bce.Hash    
import qualified Bce.BlockChain as BlockChain

import qualified Data.Binary as Bin
import GHC.Generics (Generic)
import qualified Data.Binary.Get as BinGet    
    

instance Bin.Binary Hash
instance Bin.Binary BlockChain.TxOutputRef                    
instance Bin.Binary BlockChain.TxOutput                
instance Bin.Binary BlockChain.TxInput            
instance Bin.Binary BlockChain.Transaction        
instance Bin.Binary BlockChain.BlockHeader    
instance Bin.Binary BlockChain.Block    

