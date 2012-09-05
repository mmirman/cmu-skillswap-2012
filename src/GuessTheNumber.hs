{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GuessTheNumber where

import Network.Remote.RPC

$(makeHost "Client" "localhost" 9000)
$(makeHost "ServerA" "localhost" 9001)

main = do
  putStrLn "hi"
