{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Replica (run)
import Control.Concurrent.STM (newTVarIO)
import Data.Map (empty)
import Interface.Index (index)
import Interface.Init.Handler (handlerInit)
import Network.WebSockets.Connection (defaultConnectionOptions)

main :: IO ()
main = do
  putStrLn "Serving on port 8080"
  serverStateBox <- newTVarIO empty
  run 8080 (index "Avalon") defaultConnectionOptions id $ \_ -> handlerInit serverStateBox