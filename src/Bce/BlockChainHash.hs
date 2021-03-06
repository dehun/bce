{-# LANGUAGE FlexibleInstances #-}
module Bce.BlockChainHash where

import Bce.Hash
import Bce.Crypto    
import Bce.BlockChain

import qualified Crypto.Hash.SHA256 as Sha
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL    
import qualified Data.ByteString.Lazy.Builder as BSB    
import qualified Data.ByteString.Base16 as B16
import GHC.Int(Int64, Int32)
import qualified Data.Set as Set    
import Data.Ord    

instance Hashable Block where
    hash (Block header txs) = hash header

int64ToBs :: Int64 -> BS.ByteString
int64ToBs x = BSL.toStrict $ BSB.toLazyByteString $ BSB.int64BE x

int32ToBs :: Int32 -> BS.ByteString
int32ToBs x = BSL.toStrict $ BSB.toLazyByteString $ BSB.int32BE x              

instance Hashable BlockHeader where
    hash (BlockHeader txHash prevBlockHash nonce wallclock difficulity) =
        hash (mconcat [hashBs txHash, hashBs prevBlockHash,
                       int64ToBs nonce, int64ToBs wallclock, int32ToBs difficulity])


instance Hashable (Set.Set TxInput, Set.Set TxOutput) where
    hash (inputs, outputs) = hash $ mconcat [ mconcat $ map (\i -> hashBs $ hash i) (Set.toList inputs)
                                            , mconcat $ map (\o -> hashBs $ hash o) (Set.toList outputs) ]
        
instance Hashable Transaction where
    hash (CoinbaseTransaction outputs) = hash $ mconcat $ map (\o -> hashBs $ hash  o) (Set.toList outputs)
    hash (Transaction inputs outputs sig) = hash (inputs, outputs)

                                              
instance Hashable TxOutput where
    hash (TxOutput amount pubkey) = hash $ mconcat [hashBs $ hash amount, hashBs $ hash pubkey ] 

instance Hashable TxInput where
    hash (TxInput outputRef) = hash outputRef

instance Hashable TxOutputRef where
    hash (TxOutputRef txId outputIdx) = hash $ mconcat [hashBs txId, int32ToBs outputIdx]

instance Hashable (Set.Set Transaction) where
    hash txs = hash $ mconcat $ map (hashBs . hash) (Set.toList txs)


instance Hashable PubKey where hash (PubKey bs) = hash bs
instance Hashable PrivKey where hash (PrivKey bs) = hash bs

transactionId :: Block -> Transaction -> TransactionId
transactionId block tx = hash $ mconcat $ map hashBs [hash block, hash tx]

blockId :: Block -> BlockId
blockId = hash 

-- TODO: move this away!
instance Ord Transaction where
    compare = comparing hash

instance Ord TxOutput where
    compare = comparing hash

instance Ord TxInput where
    compare = comparing hash

              
