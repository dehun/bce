module Bce.Util where

import Data.List
import Data.Word8
import Data.Maybe    
import Control.Monad    
import qualified Data.ByteString as BS

count :: (a -> Bool) -> [a] -> Int
count p xs  = length $ filter p xs

onlyOne :: (a -> Bool) -> [a] -> Bool
onlyOne p xs = 1 == count p xs

average :: (Floating a) => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

median :: (Ord a, Num a) => [a] -> a
median xs = sort xs !! (length xs `div` 2)

to01 :: Word8 -> [Word8]
to01 x' = snd $ foldl (\(x, r) _ -> (x `div` 2, (x `mod` 2) : r)) (x', []) [1..8]

to01List :: BS.ByteString -> [Word8]
to01List xs = concatMap to01 $ BS.unpack xs

zipPairs :: [a] -> [(a, a)]
zipPairs xs = zip (init xs) (tail xs)


discardResult :: IO a -> IO ()
discardResult fx = do
  fx
  return ()

secondsToMicroseconds :: Int -> Int
secondsToMicroseconds x = x * 1000000         

andM :: (Monad m, Traversable t) => t (m Bool) -> m Bool
andM xs = and <$> sequence xs

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
          
at :: [a] -> Int -> Maybe a
at xs idx = lookup idx (zip [0..] xs)

randomPick :: [a] -> Int -> Maybe a
randomPick [] _ = Nothing
randomPick xs rnd = return $ xs !! (abs rnd `mod` length xs)
