{-# LANGUAGE FlexibleInstances #-}
module CacheSpec where

import Bce.Cache
import Test.Hspec
import Test.QuickCheck    
import Test.QuickCheck.Arbitrary
import Bce.TimeStamp
    
import qualified Data.Map as Map    
import qualified Data.Set as Set    

data SmallList a = SmallList [a] deriving Show
data BigList a = BigList [a] deriving Show
instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (SmallList (a, b)) where
    arbitrary = do
      xs <- arbitrary `suchThat` (\ys -> and [ length ys == 10
                                             , length ys == (Set.size $ Set.fromList (map fst ys))])
      return $ SmallList $ xs
instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (BigList (a, b)) where               
    arbitrary = do
      xs <- arbitrary `suchThat` (\ys -> and [ length ys >= 10
                                             , length ys == (Set.size $ Set.fromList (map fst ys))])      
      return $ BigList xs

spec = parallel $ do
  describe "cache" $ do
         it "is empty at first" $ do
              cache <- createCache 10 now :: IO (Cache Int Int)
              asList cache `shouldReturn` []
         it "stores values under limit" $ property $ \lst -> do
              let SmallList xs = lst
              cache <- createCache 10 now :: IO (Cache Int String)
              mapM (\(k, v) -> cacheValue k v cache) xs
              (Set.fromList <$> asList cache) `shouldReturn` (Set.fromList xs)
         it "stores values over limit" $ property $ \lst -> do
              let BigList xs = lst
              cache <- createCache 10 now :: IO (Cache Int String)
              mapM (\(k, v) -> cacheValue k v cache) xs
              incache <- Set.fromList <$> asList cache
              Set.size incache `shouldBe` 10
              incache `shouldSatisfy` (\i -> Set.isSubsetOf i (Set.fromList xs))
    
