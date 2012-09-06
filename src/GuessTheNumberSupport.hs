module GuessTheNumberSupport where

import Network.Remote.RPC (lift, WIO)

import System.Random

newRandomInteger :: WIO w IO Integer
newRandomInteger = lift $ randomIO 