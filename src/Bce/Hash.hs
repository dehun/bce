{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bce.Hash where

type Hash = String

    

class Hashable a where
    hash :: a -> Hash

instance Hashable String where
--  TODO: repalce with some crypto hashing function    
    hash s = s
