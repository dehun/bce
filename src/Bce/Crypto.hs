{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bce.Crypto where

import Bce.Hash
import GHC.Generics        

import qualified Data.ByteString.Char8 as BS    
import qualified Data.ByteString.Base16 as B16

import qualified Crypto.Ed25519.Pure as Ec

data PubKey = PubKey BS.ByteString deriving (Eq, Ord, Generic)
data PrivKey = PrivKey BS.ByteString deriving (Eq, Ord, Generic)

data KeyPair = KeyPair { keyPairPub :: PubKey
                       , keyPairPriv :: PrivKey } deriving (Ord, Eq, Generic, Show)

instance Show PubKey
    where show (PubKey x) = BS.unpack $ B16.encode x

instance Read PubKey where
    readsPrec _ input = let (d, r) = B16.decode $ BS.pack input
                       in if d == BS.empty
                          then []
                          else [(PubKey d, BS.unpack r)]                            


instance Show PrivKey
    where show (PrivKey x) = BS.unpack $ B16.encode x

instance Read PrivKey where
    readsPrec _ input = let (d, r) = B16.decode $ BS.pack input
                       in if d == BS.empty
                          then []
                          else [(PrivKey d, BS.unpack r)]                            
                               
type Signature = BS.ByteString

sign :: Hash -> PrivKey -> Signature
sign (Hash h) (PrivKey k) =
    let Just privKey = Ec.importPrivate k
        pubKey = Ec.generatePublic privKey
        Ec.Sig sig = Ec.sign h privKey pubKey
    in sig

verifySignature :: Signature -> PubKey -> Hash -> Bool
verifySignature s (PubKey p) (Hash h) =
    let Just pubKey = Ec.importPublic p
    in Ec.valid h pubKey (Ec.Sig s)
