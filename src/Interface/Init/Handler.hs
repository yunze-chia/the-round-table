{-# LANGUAGE OverloadedStrings #-}

module Interface.Init.Handler where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Map (insert, (!?))
import Interface.Init.Render (renderInit)
import Interface.Room.Handler (handlerRoom)
import Interface.Types (Room (WaitingRoom), RoomMeta (RoomMeta), Server)

handlerInit :: TVar Server -> Widget HTML a
handlerInit serverVar = do
  (roomId, playerName) <- renderInit "" "" ""
  RoomMeta roomVar roomChan <- liftIO . atomically $ do
    serverState <- readTVar serverVar
    case serverState !? roomId of
      Just (RoomMeta roomVar roomChan) -> return $ RoomMeta roomVar roomChan
      Nothing -> do
        waitChan <- newBroadcastTChan
        waitVar <- newTVar []
        roomVar <- newTVar $ WaitingRoom waitVar waitChan
        roomChan <- newBroadcastTChan
        modifyTVar' serverVar $ insert roomId $ RoomMeta roomVar roomChan
        return $ RoomMeta roomVar roomChan
  handlerRoom roomId playerName roomVar =<< (liftIO . atomically $ dupTChan roomChan)
