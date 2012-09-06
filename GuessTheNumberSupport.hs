module GuessTheNumberSupport where

import Network.Remote.RPC (lift, WIO)
import Data.Int
import System.Random

newRandomInteger :: WIO w IO Int8
newRandomInteger = lift $ randomIO 

compareTwo :: (Int8, Int8) -> Ordering
compareTwo = uncurry compare 

printLine :: String -> WIO w IO ()
printLine str = lift $ putStrLn str

readLine :: Read a => WIO w IO a
readLine = lift $ readLn

repeateWhileTrue m = m'
  where m' = do 
          b <- m
          if b 
            then m' 
            else return ()


setActionValue a = return a