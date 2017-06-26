module Bce.Crypto where

import Bce.Hash

import qualified Data.ByteString as BS

type PubKey = String

type Signature = BS.ByteString    

-- TODO: implement me
sign :: Hash -> Signature
sign h = hashBs h

verifySignature :: Signature -> Hash -> Bool
verifySignature s h = s == hashBs h
