module Interface.Room.Handler where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Engine.State
import Interface.Game.Handler (handlerGame)
import Interface.Room.Render (renderBounced, renderLobby, renderRoom, renderRoomInfo)
import Interface.Types (PlayerName, Room (GameRoom, WaitingRoom), RoomId)
import Lens.Micro.Platform (each, (^..))

handlerRoom :: RoomId -> PlayerName -> TVar Room -> TChan () -> Widget HTML a
handlerRoom roomId playerName roomVar roomChan =
  renderRoom (renderRoomInfo roomId playerName) (handlerRoomMain playerName roomVar roomChan)

handlerRoomMain :: PlayerName -> TVar Room -> TChan () -> Widget HTML a
handlerRoomMain playerName roomVar roomChan = do
  room <- liftIO . atomically $ do
    roomState <- readTVar roomVar
    case roomState of
      GameRoom gameVar _ -> do
        game <- readTVar gameVar
        return $ if playerName `elem` (game ^.. players . each . name) then Just roomState else Nothing
      WaitingRoom waitVar waitChan -> do
        lobby <- readTVar waitVar
        if playerName `elem` lobby
          then return $ Just roomState
          else do
            let newLobby = lobby ++ [playerName]
            writeTVar waitVar newLobby
            writeTChan waitChan ()
            return $ Just roomState
  let updateLoop widget = do
        widget <|> (liftIO . atomically $ readTChan roomChan)
        handlerRoomMain playerName roomVar roomChan
  case room of
    Nothing -> renderBounced
    Just (GameRoom gameVar gameChan) -> updateLoop $ handlerGame playerName gameVar =<< (liftIO . atomically $ dupTChan gameChan)
    Just (WaitingRoom waitVar waitChan) -> updateLoop $ handlerWaitRoom playerName waitVar =<< (liftIO . atomically $ dupTChan waitChan)

handlerWaitRoom :: PlayerName -> TVar [PlayerName] -> TChan () -> Widget HTML a
handlerWaitRoom playerName waitVar waitChan = do
  lobby <- liftIO . readTVarIO $ waitVar
  let receiveUpdate = liftIO . atomically . readTChan $ waitChan
  receiveUpdate <|> renderLobby lobby
  handlerWaitRoom playerName waitVar waitChan
