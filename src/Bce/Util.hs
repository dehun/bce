module Bce.Util where

import Data.List

count :: (a -> Bool) -> [a] -> Int
count p xs  = length $ filter p xs

onlyOne :: (a -> Bool) -> [a] -> Bool
onlyOne p xs = 1 == count p xs
