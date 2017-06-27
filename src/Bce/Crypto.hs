module Bce.Crypto where

import Bce.Hash

import qualified Data.ByteString as BS

type PubKey = BS.ByteString
type PrivKey = BS.ByteString    

type Signature = BS.ByteString    

-- TODO: implement me
sign :: Hash -> PrivKey -> Signature
sign h k = hashBs h

verifySignature :: Signature -> PubKey -> Hash -> Bool
verifySignature s p h = s == hashBs h
