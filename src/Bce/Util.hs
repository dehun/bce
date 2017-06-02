module Bce.Util where

import Data.List

count :: (a -> Bool) -> [a] -> Int
count p xs  = length $ filter p xs

onlyOne :: (a -> Bool) -> [a] -> Bool
onlyOne p xs = 1 == count p xs

average :: (Floating a) => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

median :: (Ord a, Num a) => [a] -> a
median xs = sort xs !! (length xs `div` 2)

zipPairs :: [a] -> [(a, a)]
zipPairs xs = zip (init xs) (tail xs)
