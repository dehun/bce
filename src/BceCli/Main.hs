import Control.Monad
import Data.Either    

import Command
import System.IO    
    

shellPrefix = "[_]>> "

processCmd cmd = putStrLn $ show cmd
              
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
         putStr shellPrefix
         cmdLine <- getLine
         case parseCommand cmdLine of
           Left err -> do
              putStrLn $ "wrong input: " ++ show err
           Right cmd -> do
              processCmd cmd
                        
         
