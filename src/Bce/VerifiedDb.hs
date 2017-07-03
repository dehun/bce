module Bce.VerifiedDb where

import Bce.BlockChain
import Bce.BlockChainVerification
import Bce.Logger    
import qualified Bce.DbFs as Db
    

import Data.Either
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import qualified Data.Set as Set    


verifyAndPushBlocks :: Db.Db -> [Block] -> IO ()
verifyAndPushBlocks db blocks = mapM_ (verifyAndPushBlock db) (reverse blocks) -- starting from oldest

verifyAndPushBlock :: Db.Db -> Block -> IO Bool
verifyAndPushBlock db block = do
    verificationResult <- runEitherT $ verifyBlock db block
    case  verificationResult of
      Right _ -> do
        Db.pushBlock db block
      Left err -> do
        logWarning $ "verification  pushing block failed: " ++ err
        return False


verifyAndPushTransactions :: Db.Db -> Set.Set Transaction -> IO (Either String ())
verifyAndPushTransactions db txs = runEitherT $ do
  mapM (verifyTransactionTransaction db) $ Set.toList txs
  liftIO $ Db.pushTransactions db txs


               
