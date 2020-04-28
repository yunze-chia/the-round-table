{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Replica                (run)
import Control.Concurrent.STM        (newTVarIO)
import Data.Map                      (empty)
import Interface.Init.Handler        (handlerInit)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Interface.Index               (indexWithBootstrap)

main :: IO ()
main = do
  serverStateBox <- newTVarIO empty
  run 8080 (indexWithBootstrap "Avalon") defaultConnectionOptions id $ \_ -> handlerInit serverStateBox