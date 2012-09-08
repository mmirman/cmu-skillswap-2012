{-# LANGUAGE TemplateHaskell #-}
module Main where

-- The imports bring functions and datatypes into the namespace
import Network.Remote.RPC
import GuessTheNumberSupport
import System.Environment

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
newGameServer = do -- "do" is a keyword a bit like "{" but without a matching "}"
  -- first we declare that this action was intended to run on the host "Keeper"
  onHost Keeper
  -- the next line assigns the new variable r to be a random Integer.
  r <- newRandomInteger
  -- numberGuesser is a closure.  It captures the variable r and uses it in a function 
  -- which leaves the scope of this action.
  let numberGuesser(k) = compareTwo(k,r)
  -- while haskell has the word "return", it is just an ordinary function.  Rather than     
  -- exiting the code block with the given value, "return" just sets the resulting value 
  -- of the code block at this point to the given value.  In fact, every line in a "do"
  -- block sets the resulting value of the code block at that point to "a" value, return just
  -- lets you set it manually.
  return numberGuesser
  -- All three above lines can be written "flip compare <$> newRandomInteger" 
  -- instead. Don't worry too much.

guesserClient = do
  onHost Guesser
  
  -- here we actually make a call to newGameServer to get an instance of the game.
  -- the function never actually leaves the server, 
  -- it just allows us to make calls to the server.
  guessingInstance <- $(rpcCall 'newGameServer) 
                        
  repeateWhileTrue $ do 
    printLine "guess a number!"
    answer <- readLine -- haskell type infers what you type you want to read from the terminal
    result <- guessingInstance( answer ) -- the parens here are unecessary.
    case result of 
      -- remember, we aren't exiting the action when we write "return", 
      -- just saying that the do block in the while statement executed to true or false.
      EQ -> return False  -- break
      LT -> do printLine "you guessed low.  guess again"
               return True -- continue
      GT -> do printLine "you guessed high.  guess again"
               return True -- continue
  
  printLine "you guessed correctly! want to play again? Input \"True\" or \"False\""
  answer <- readLine
  if answer 
    then guesserClient -- play again by calling recursivly.
    else printLine "thanks for playing."

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
