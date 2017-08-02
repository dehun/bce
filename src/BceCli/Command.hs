module Command where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Bce.BlockChain
import Bce.Hash    

type WalletId = Hash    

data Command = QueryBalance WalletId
             | CreateWallet
             | ListWallets
             | PerformTransaction { txSender :: WalletId
                                  , txReceiver:: WalletId
                                  , txAmount :: Int }
             | ShowHead
             | ShowBlock { showBlockId :: BlockId }
             | ShowTransaction {showTransactionId :: TransactionId }
             | Shell
               deriving (Show, Eq)

intParser = read <$> many1 digit
hashParser = read <$> many1 alphaNum                        
walletIdParser = hashParser
blockIdParser = hashParser
txIdParser = hashParser


walletCommandParser = do
  spaces
  (do string "balance" 
      walletId <- spaces >> walletIdParser
      return $ QueryBalance walletId)
   <|> (do string "transaction" >> spaces
           sender <- spaces >> walletIdParser
           receiver <- walletIdParser
           amount <- spaces >> intParser
           return $ PerformTransaction sender receiver amount)
   <|> (string "create" >> return CreateWallet)
   <|> (string "list" >> return ListWallets)

blockChainCommandParser = do
  spaces
  (string "show" >> spaces >>
              ((string "head" >> return ShowHead)
               <|> (do string "transaction"
                       txId <- spaces >> txIdParser
                       return $ ShowTransaction txId)
              <|> (do string "block"
                      blockId <- spaces >> blockIdParser
                      return $ ShowBlock blockId)))
  
commandParser = do
  spaces
  (eof >> return Shell)
   <|> (string "chain" >> blockChainCommandParser)
   <|> (string "wallet" >> walletCommandParser)
                  
                        
parseCommand :: String -> Either ParseError Command
parseCommand inp = parse commandParser "command" inp

  
