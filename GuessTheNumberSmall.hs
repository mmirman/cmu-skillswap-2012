{-# LANGUAGE TemplateHaskell #-}
module Main where

-- Note that this all fits into 48 easy to read lines!  
-- How long do you think the java equivalent would be?

import Network.Remote.RPC
import GuessTheNumberSupport
import System.Environment
import Data.Functor

$(makeHost "Keeper" "localhost" 9001) -- the server
$(makeHost "Guesser" "localhost" 9000) -- the client

newGameServer = do 
  onHost Keeper 
  flip compare <$> newRandomInteger

guesserClient = do
  onHost Guesser
  guessingInstance <- $(rpcCall 'newGameServer)
                        
  repeateWhileTrue $ do 
    printLine $ "guess a number! between " ++ minBound (1 :: Int8) ++ ", " ++ maxBound (1 :: Int8) 
    answer <- readLine
    result <- guessingInstance answer 
    case result of 
      EQ -> return False
      LT -> do printLine "you guessed low.  guess again"
               return True
      GT -> do printLine "you guessed high.  guess again"
               return True
  
  printLine "you guessed correctly! want to play again? Input \"True\" or \"False\""
  answer <- readLine
  if answer 
    then guesserClient
    else printLine "thanks for playing."

main = do
  args <- getArgs
  case args of 
    ["-server"] -> runServer $(autoService 'Keeper)
    ["-client"] -> runServer guesserClient
    _ -> do
      putStrLn "running both the client and the server. -server or -client"
      runServerBG $(autoService 'Keeper)
      runServer guesserClient