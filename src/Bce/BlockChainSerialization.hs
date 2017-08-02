{-# LANGUAGE OverloadedStrings #-}
module Bce.BlockChainSerialization where

import Bce.Hash
import Bce.Crypto    
import qualified Bce.BlockChain as BlockChain
import Bce.BlockChainHash

import qualified Data.Binary as Bin
import GHC.Generics (Generic)
import qualified Data.Binary.Get as BinGet

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding    
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16    
    

instance Bin.Binary Hash
instance Bin.Binary PubKey
instance Bin.Binary PrivKey
instance Bin.Binary KeyPair    
instance Bin.Binary BlockChain.TxOutputRef                    
instance Bin.Binary BlockChain.TxOutput                
instance Bin.Binary BlockChain.TxInput            
instance Bin.Binary BlockChain.Transaction        
instance Bin.Binary BlockChain.BlockHeader    
instance Bin.Binary BlockChain.Block

instance Aeson.ToJSON BS.ByteString where
    toJSON bs = Aeson.toJSON (TextEncoding.decodeUtf8 $ B16.encode bs)

instance Aeson.FromJSON BS.ByteString where
    parseJSON (Aeson.String txt) = return $ fst $ B16.decode $ TextEncoding.encodeUtf8 txt


instance Aeson.FromJSON Hash
instance Aeson.FromJSON PubKey
instance Aeson.FromJSON PrivKey        
instance Aeson.FromJSON BlockChain.TxOutputRef
instance Aeson.FromJSON BlockChain.TxOutput                
instance Aeson.FromJSON BlockChain.TxInput            
instance Aeson.FromJSON BlockChain.Transaction        
instance Aeson.FromJSON BlockChain.BlockHeader    
instance Aeson.FromJSON BlockChain.Block

instance Aeson.ToJSON Hash
instance Aeson.ToJSON PubKey
instance Aeson.ToJSON PrivKey        
instance Aeson.ToJSON BlockChain.TxOutputRef                    
instance Aeson.ToJSON BlockChain.TxOutput                
instance Aeson.ToJSON BlockChain.TxInput            
instance Aeson.ToJSON BlockChain.Transaction        
instance Aeson.ToJSON BlockChain.BlockHeader    
instance Aeson.ToJSON BlockChain.Block

