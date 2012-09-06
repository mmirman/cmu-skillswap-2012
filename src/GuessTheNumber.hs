{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GuessTheNumber where

import Network.Remote.RPC
import GuessTheNumberSupport

$(makeHost "Seeker" "localhost" 9000)
$(makeHost "Keeper" "localhost" 9001)

seeker = do
  onHost Seeker

keeper = do
  
  
main = do
  putStrLn "hi"
