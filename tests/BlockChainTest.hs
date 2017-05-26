module BlockChainTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import BlockChain

prop_elemAt :: [Char] -> Int -> Bool
prop_elemAt xs idx =
    if and [ idx < length xs, idx >= 0 ]
    then xs !! idx == elemAt xs idx
    else elemAt xs idx == Nothing
    
