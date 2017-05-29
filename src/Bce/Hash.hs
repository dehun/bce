{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Hash where

type Hash = String

    

class Hashable a where
    hash :: a -> Hash

instance Hashable String where
--  TODO: repalce with some crypto hashing function    
    hash s = s
