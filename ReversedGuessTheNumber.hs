{-# LANGUAGE TemplateHaskell #-}
module Main where

-- The imports bring functions and datatypes into the namespace
import Network.Remote.RPC
import GuessTheNumberSupport
import System.Environment
import GHC.Int
-- Note 1:  Haskell is not a scripting language. 
-- Like Java, the order of top level declaration does not matter.

-- Note 2: Haskell syntax can be whitespace dependent.  
-- The rules for this are easy! Just write your code how you would 
-- in an imperative C syntax style language with semicolons 
-- and curlybraces, keeping logical code blocks grouped by indentation 
-- (or spaces), then remove the semicolons and curlybraces. 
-- If this bothers you, just leave in the semicolons and curlybraces! 

-- Fist, the worlds which services run on need to be declared.
-- for now they will run on different ports only, but we could 
-- change their hosts.
$(makeHost "Keeper" "localhost" 9001) -- the server
$(makeHost "Guesser" "localhost" 9000) -- the client

-- Note 3: newGameServer is an "action".  This is to haskell what a zero argument
-- function/method would be in most other languages.  Rather than calling it
-- by writing "newGameServer();"  we only have to write "newGameServer". 

-- when newGameServer gets called, it returns a function representing 
-- a new instance of the game!  The function it returns compares a guessed number against  
-- the server's number.
gameServer :: (Int8 -> WIO Keeper IO Ordering) -> WIO Keeper IO ()
gameServer foo = do 
  onHost Keeper
  let binarySearch(low,high) = do
        let next = (low + high) `div` 2
        was_correct <- foo next    
        case was_correct of 
          EQ -> printLine $ "yay! " ++ show next
          LT -> binarySearch(next,high)
          GT -> binarySearch(low,next)
  binarySearch bounds

guesserClient :: WIO Guesser IO ()
guesserClient = do 
  onHost Guesser
  printLine "give me a number"
  yournumber <- readLine 
  $(rpcCall 'gameServer) $ \r -> compareTwo(r,yournumber)


-- don't worry too much about what is going on in the main file.            
main = do
  args <- getArgs
  case args of 
    ["-server"] -> runServer $(autoService 'Keeper)
    ["-client"] -> runServer guesserClient
    _ -> do
      putStrLn "running both the client and the server. -server or -client"
      -- find all services that should run on the host Keeper, 
      -- and runs them on a background server
      runServerBG $(autoService 'Keeper)
      -- run the client as a forground process
      runServer guesserClient
