{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Main where

import Network.Remote.RPC
import GuessTheNumberSupport

$(makeHost "Guesser" "localhost" 9000)
$(makeHost "Keeper" "localhost" 9001)

guesserClient = do
  onHost Guesser
  guessingInstance <- $(rpcCall 'newKeeperInstanceServer)
                        
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
  

newKeeperInstanceServer = do
  onHost Keeper
  r <- newRandomInteger
  let numberGuesser(k) = compareTwo(k,r) 
  return numberGuesser 
          
main = do
   -- find all services that should run on the host Keeper, and runs them on a background server
  runServerBG $(autoService 'Keeper)
  -- run the client as a forground process
  runServer guesserClient
