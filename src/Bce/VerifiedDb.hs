module Bce.VerifiedDb where

import Bce.BlockChain
import Bce.BlockChainVerification
import Bce.Logger    
import qualified Bce.DbFs as Db
    

import Data.Either
import Data.List
import Data.Maybe    
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import qualified Data.Set as Set    


verifyAndPushBlocks :: Db.Db -> [Block] -> IO ()
verifyAndPushBlocks db blocks = mapM_ (verifyAndPushBlock db) (reverse blocks) -- starting from oldest

verifyAndPushBlock :: Db.Db -> Block -> IO Bool
verifyAndPushBlock db block =
  Db.transactionally db $ do
    verificationResult <- runEitherT $ verifyBlock db block
    case verificationResult of
      Right vblk -> do
        Db.pushBlock db vblk
      Left err -> do
        logWarning $ "verification  pushing block failed: " ++ err
        return False


verifyAndPushTransactions :: Db.Db -> Set.Set Transaction -> IO (Either String ())
verifyAndPushTransactions db txs = Db.transactionally db $ do
  let txIns = concatMap (Set.toList . txInputs) txs
  alreadyInMem <- Set.toList <$> Db.getTransactions db
  let isAlreadyUsed = find (\t -> case t of
                                    CoinbaseTransaction _ -> False
                                    Transaction ins outs sig ->
                                     any (\inp -> Set.member inp ins) txIns
                           ) alreadyInMem
  if isJust isAlreadyUsed
  then return $ Left "transaction clashes with already in-memory transactions "
  else do
     runEitherT $ do
              vtxs <- mapM (verifyTransactionTransaction db) $ Set.toList txs
              liftIO $ Db.pushTransactions db $ Set.fromList vtxs
              right ()


               
