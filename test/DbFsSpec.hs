module DbFsSpec where

import Test.Hspec
import System.Directory
import Control.Exception    
    
import qualified Bce.DbFs as Db
import Bce.InitialBlock    
import Bce.Miner    
    

testDbPath = "./tmpdb"

flushDb db = do
  removeDirectoryRecursive (Db.dbDataDir db)

withDb :: String -> (Db.Db -> IO()) -> IO ()
withDb path = bracket (Db.initDb path) flushDb
  

spec :: Spec
spec = do
  around (withDb testDbPath) $ do 
         describe "DbFs" $ do
           it "works" $ \db -> do
                               Db.getLongestHead db `shouldReturn` (1, initialBlock)
