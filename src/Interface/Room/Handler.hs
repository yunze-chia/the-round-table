{-# LANGUAGE OverloadedStrings #-}

module Interface.Room.Handler where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Engine.Game (newGame)
import Engine.Helpers (defaultGameConfig)
import Engine.State (GameConfig, name, players)
import Interface.Game.Config.Render (renderGameConfig)
import Interface.Game.Handler (handlerGame)
import Interface.Room.Render (renderBounced, renderLobby, renderLobbyGamemaster, renderRoom, renderRoomInfo)
import Interface.Types (Action (ActionValue, Update), PlayerName, Room (GameRoom, WaitingRoom), RoomId)
import Lens.Micro.Platform (each, (^..))
import System.Random (newStdGen)

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
      receiveUpdate = Update <$ (liftIO . atomically . readTChan $ roomChan)
      loop = handlerRoomMain playerName roomVar roomChan
  case room of
    Nothing -> renderBounced
    Just (GameRoom gameVar gameChan) -> updateLoop $ handlerGame playerName gameVar =<< (liftIO . atomically $ dupTChan gameChan)
    Just (WaitingRoom waitVar waitChan) -> do
      waitChan' <- liftIO . atomically $ dupTChan waitChan
      let receiveGameStart = ActionValue <$> handlerLobby playerName waitVar waitChan'
      action <- receiveUpdate <|> receiveGameStart
      case action of
        Update -> loop
        ActionValue gameConfig -> do
          randomGen <- liftIO newStdGen
          liftIO . atomically $ do
            lobby <- readTVar waitVar
            gameVar <- newTVar $ newGame randomGen gameConfig lobby
            gameChan <- newBroadcastTChan
            writeTVar roomVar $ GameRoom gameVar gameChan
            writeTChan roomChan ()
            readTChan roomChan
          loop

data LobbyWidgetAction = LobbyStart GameConfig | LobbyUpdate

handlerLobby :: PlayerName -> TVar [PlayerName] -> TChan () -> Widget HTML GameConfig
handlerLobby playerName waitVar waitChan = do
  lobby <- liftIO . readTVarIO $ waitVar
  let loop = handlerLobby playerName waitVar waitChan
      receiveUpdate = liftIO . atomically . readTChan $ waitChan
  if head lobby /= playerName
    then do
      receiveUpdate <|> renderLobby lobby
      loop
    else do
      let receiveLobbyUpdate = LobbyUpdate <$ receiveUpdate
          receiveGameStart =
            renderLobbyGamemaster
              (renderLobby lobby)
              (LobbyStart <$> renderGameConfig lobby "" defaultGameConfig)
      action <- receiveLobbyUpdate <|> receiveGameStart
      case action of
        LobbyUpdate -> loop
        LobbyStart gameConfig -> return gameConfig
