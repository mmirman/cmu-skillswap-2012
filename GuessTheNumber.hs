{-# LANGUAGE TemplateHaskell #-}
module Main where

import Network.Remote.RPC
import GuessTheNumberSupport
import System.Environment

$(makeHost "Hider" "localhost" 9001)
$(makeHost "Guesser" "localhost" 9000)

guesserClient = do
  onHost Guesser
  guessingInstance <- $(rpcCall 'newGameServer)
                        
  repeateWhileTrue $ do 
    printLine "guess a number!"
    answer <- readLine
    result <- guessingInstance( answer ) 
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

newGameServer = do
  onHost Hider
  r <- newRandomInteger
  let numberGuesser(k) = compareTwo(k,r) 
  return numberGuesser
          
main = do
  args <- getArgs
  case args of 
    ["-server"] -> runServer $(autoService 'Hider)
    ["-client"] ->  runServer guesserClient
    _ -> do
      putStrLn "running both the client and the server. -server or -client"
      -- find all services that should run on the host Keeper, and runs them on a background server
      runServerBG $(autoService 'Hider)
      -- run the client as a forground process
      runServer guesserClient
