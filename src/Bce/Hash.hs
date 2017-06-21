{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bce.Hash where

import qualified Crypto.Hash.SHA256 as Sha
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL    
import qualified Data.ByteString.Lazy.Builder as BSB    
import qualified Data.ByteString.Base16 as B16
import GHC.Int(Int64)
import GHC.Generics (Generic)
import Data.Binary    

data Hash = Hash { hashBs :: BS.ByteString } deriving (Eq, Ord, Generic)

instance Show Hash where
    show (Hash h) = BS.unpack $ B16.encode h

class Hashable a where
    hash :: a -> Hash

instance Hashable Hash where
    hash (Hash h) = Hash $ Sha.hash h

instance Hashable String where
    hash s = Hash $ Sha.hash $ BS.pack s

instance Hashable BSL.ByteString where
    hash bs = Hash $ Sha.hash $ BSL.toStrict bs

instance Hashable BS.ByteString where
    hash bs = Hash $ Sha.hash  bs                           

instance Hashable Int64 where
    hash i = hash $ BSB.toLazyByteString $ BSB.int64BE i


        
