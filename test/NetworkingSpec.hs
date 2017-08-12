module NetworkingSpec where
    
import qualified Bce.Networking as Networking
import qualified Bce.VerifiedDb as VerifiedDb
import qualified Bce.P2p as P2p
import qualified Bce.DbFs as Db
import Bce.Crypto
import Bce.Verified
import Bce.BlockChainHash
import Bce.BlockChain    
import Bce.Util
import Bce.PeerAddress

import Test.Hspec    
import Test.QuickCheck    
import Test.QuickCheck.Arbitrary
import Data.List
import Data.Maybe    
import qualified Data.Set as Set
import Control.Concurrent
import Control.Monad.Extra    
import Data.Ord
    
import ArbitraryDb

data WithArbitraryNetworks = WithArbitraryNetworks {
      runWithArbNetworks :: ([Networking.Network] -> IO () ) -> IO ()
    , networksCount :: Int
    , keyPairs :: Set.Set KeyPair}
instance Show WithArbitraryNetworks where
    show g = "arbitrary " ++ show (networksCount g)  ++ " networks generator"


instance Arbitrary WithArbitraryNetworks where
    arbitrary = do
        nNetworks <- choose (2, 8)
        let networkIds = [1..nNetworks]
        portRangeStart <- choose (16000, 32000)
        portRangeEnd <- choose (46000, 56000)
        freePorts <- mapM (\_ -> choose (portRangeStart, portRangeEnd) :: Gen Int) networkIds
        let ports = zip networkIds freePorts
        let nidToPeerAddress nid = PeerAddress ("(127,0,0," ++ show (10+nid) ++ ")")
                                                   (fromIntegral $ fromJust $ lookup nid ports)
        -- minimal spanning tree for fully connected graph is shuffled list of vertexes paired
        shuffled <-  shuffle networkIds
        let mst = (last shuffled, head shuffled) : zip (init shuffled) (tail shuffled) 
        networkWithers <- mapM (\nid -> do
                              let peerAddress = nidToPeerAddress nid
                              let config = P2p.P2pConfig peerAddress 1 1 1 64
                              extraSeedsCount <- choose (0, nNetworks - 1)
                              let Just keySeed = lookup nid mst
                              extra <- take extraSeedsCount <$> shuffle networkIds
                              let seeds = map nidToPeerAddress $ keySeed : extra
                              dbFiller <- arbitrary
                              return (dbFiller, (\fx ->
                                          withArbitraryDb (\db -> do
                                              (dbFillerRun dbFiller) db
                                              putStrLn $ "starting network on " ++ show peerAddress 
                                              net <- Networking.start config seeds db
                                              fx net
                                              Networking.stop net)))
                         ) networkIds
--      :: [(Network -> IO ()) -> IO ()] -> (([Network] -> IO ()) -> IO ())
        let withNetworks = foldl (\acc w -> (\fx -> w (\n -> acc (\ns -> fx (n:ns)))))
                           (\fx -> fx []) $ map snd networkWithers
        let allKeys = foldl (\acc w -> Set.union acc (dbFillerKeys $  fst w)) Set.empty networkWithers
        return $ WithArbitraryNetworks withNetworks nNetworks allKeys


waitCondition :: Int -> IO Bool -> IO ()
waitCondition timeout condition  =
    continue 0
    where continue alreadyPassed
              | alreadyPassed > timeout = return ()
              | otherwise = do
            c <- condition
            if c then return ()
            else do
              let delay = 1
              threadDelay $ secondsToMicroseconds delay
              continue $ alreadyPassed + delay
               

spec :: Spec    
spec = parallel $ do
  describe "Networking" $ do
    it "networking sync blocks" $ property $ \withNetworks -> do
        (runWithArbNetworks withNetworks) $ \nets -> do
               let dbs = map Networking.networkDb nets
               let getDbLengths = mapM (\db -> do
                                    (l, _) <- Db.getLongestHead db
                                    return l) dbs
               dbLengths <- getDbLengths
               putStrLn "initial db lengths are: "
               putStrLn $ show dbLengths                            
               let longest = maximum dbLengths
               putStrLn $ "longest is " ++ show longest
               waitCondition 25 $ do
                            ls <- getDbLengths
                            return $ longest == minimum ls
               newDbLengths <- getDbLengths
               putStrLn "final db lengths are: "
               putStrLn $ show newDbLengths
               mapM_ (\nl -> nl `shouldSatisfy` (==longest)) newDbLengths
    it "networking sync transactions" $ property $ \withNetworks -> do
        pendingWith "fails on travis"
        (runWithArbNetworks withNetworks) $ \nets -> do
            let dbs = map Networking.networkDb nets
            let getDbLengths = mapM (\db -> do
                                       (l, _) <- Db.getLongestHead db
                                       return (db, l)) dbs                      
            dbLengths <- getDbLengths
            let (maxDb, longest) = maximumBy (comparing snd) dbLengths
            (_, VerifiedBlock topBlock) <- Db.getLongestHead maxDb
            unspent <- Db.unspentAt maxDb $ blockId topBlock
            Just (tx, _) <- generateArbitraryTx maxDb unspent $ Set.toList $ keyPairs withNetworks
            VerifiedDb.verifyAndPushTransactions maxDb $ Set.singleton tx
            Db.getTransactions maxDb `shouldReturn` Set.singleton tx
            waitCondition 25 $ do             -- wait for blockchains to sync
                            ls <- map snd <$> getDbLengths
                            return $ longest == minimum ls
            let condition = all (==[tx]) <$> mapM (\db -> Set.toList <$> Db.getTransactions db) dbs
            waitCondition 50 condition             -- wait for transactions to sync
            condition `shouldReturn` True
                               
               
         
