module Bce.Cache where

import Bce.TimeStamp
    
import Data.IORef
import Data.Ord    
import Data.Maybe
import Data.List    
import Control.Monad
import qualified Data.Map as Map    

data CacheEntry v = CacheEntry { cacheEntryValue :: v
                               , cacheEntryAccessedAt :: TimeStamp}
    
data Cache k v = Cache { cacheState :: IORef (Map.Map k (CacheEntry v))
                       , cacheLimit :: Int}

createCache :: (Ord k) => Int -> IO (Cache k v)
createCache limit = Cache <$> newIORef Map.empty <*> pure limit

cacheValue :: (Ord k) => k -> v -> Cache k v -> IO (Cache k v)
cacheValue k v cache = do
    oldState <- readIORef $ cacheState cache
    t <- now
    let newState = Map.insert k (CacheEntry v t) oldState
    writeIORef  (cacheState cache) newState
    when (Map.size newState > cacheLimit cache) (pruneCache cache)
    return cache
    
pruneCache :: (Ord k) => Cache k v -> IO ()
pruneCache cache = do
  t <- now
  s <- readIORef $ cacheState cache
  let all = Map.assocs s
  let remaining = take (cacheLimit cache) $ reverse $ sortBy (comparing (cacheEntryAccessedAt . snd)) all
  let ns = Map.fromList remaining
  writeIORef (cacheState cache) ns

queryCache :: (Ord k) => k -> Cache k v -> IO (Maybe v)
queryCache k cache = do
  s <- readIORef $ cacheState cache
  case Map.lookup k s of
    Nothing -> return Nothing
    Just entry -> do
        t <- now
        let newEntry = entry {cacheEntryAccessedAt=t}
        writeIORef (cacheState cache) (Map.insert k newEntry s)
        return $ Just $ cacheEntryValue entry


asList :: (Ord k) => Cache k v -> IO [(k, v)]
asList cache = map (\(k, e) -> (k, cacheEntryValue e)) <$> Map.assocs <$> readIORef (cacheState cache)

               
