{-# LANGUAGE OverloadedStrings #-}

module Interface.Gamemaster.Handler where

import Concur.Core                        (Widget)
import Concur.Replica                     (HTML)
import Control.Applicative                ((<|>))
import Control.Concurrent.STM             (atomically)
import Control.Concurrent.STM.TChan       (TChan, dupTChan, newBroadcastTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar        (TVar, modifyTVar', newTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class             (liftIO)
import Engine.Game                        (newGame)
import Engine.Helpers                     (defaultGameConfig)
import Engine.State
import Interface.Gamemaster.Board.Render  (renderBoard, renderLeaderSequence, renderQuestHistory, renderVoteHistory)
import Interface.Gamemaster.Config.Render (renderGameConfig, renderKick, renderReturn)
import Interface.Gamemaster.Render        (renderRoomInfoGM, renderWaitRoomGM)
import Interface.Room.Render              (renderLobby, renderRoom)
import Interface.Types                    (Action (Action, ActionValue, Update), PlayerName, Room (GameRoom, WaitingRoom), RoomId)
import Lens.Micro.Platform                (each, (^.), (^..))
import System.Random                      (newStdGen)

handlerGamemaster :: RoomId -> TVar Room -> TChan () -> Widget HTML a
handlerGamemaster roomId roomStateVar updateChan =
  renderRoom (renderRoomInfoGM roomId) (handlerGamemasterMain roomStateVar updateChan)

handlerGamemasterMain :: TVar Room -> TChan () -> Widget HTML a
handlerGamemasterMain roomVar roomChan = do
  let receiveUpdate = Update <$ (liftIO . atomically . readTChan $ roomChan)
      loop = handlerGamemasterMain roomVar roomChan
  room <- liftIO . readTVarIO $ roomVar
  case room of
    GameRoom gameVar gameChan -> do
      handlerGamemasterGameRoom gameVar =<< (liftIO . atomically $ dupTChan gameChan)
      liftIO . atomically $ do
        game <- readTVar gameVar
        waitVar <- newTVar $ game ^. players ^.. each . name
        waitChan <- newBroadcastTChan
        writeTVar roomVar $ WaitingRoom waitVar waitChan
        writeTChan roomChan ()
        readTChan roomChan
      loop
    WaitingRoom waitVar waitChan -> do
      waitChan' <- liftIO . atomically $ dupTChan waitChan
      let receiveAction = ActionValue <$> handlerGamemasterWaitRoom waitVar waitChan'
      action <- receiveUpdate <|> receiveAction
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

handlerGamemasterGameRoom :: TVar Game -> TChan () -> Widget HTML ()
handlerGamemasterGameRoom gameVar gameChan = do
  game <- liftIO . readTVarIO $ gameVar
  let receiveUpdate = Update <$ (liftIO . atomically . readTChan $ gameChan)
      receiveAction =
        Action
          <$ renderBoard
            (if isGameOver $ game ^. present then renderReturn else renderLeaderSequence (game ^. players ^.. each . name))
            (renderQuestHistory [p | p <- game ^. history ++ [game ^. present], isEndPhase p])
            (renderVoteHistory (game ^. players ^.. each . name) $ game ^. history ++ [game ^. present])
  action <- receiveUpdate <|> receiveAction
  case action of
    Update -> handlerGamemasterGameRoom gameVar gameChan
    Action -> return ()

data GMWaitRoomWidgetAction = GMWaitRoomKick PlayerName | GMWaitRoomStart GameConfig | GMWaitRoomUpdate

handlerGamemasterWaitRoom :: TVar [PlayerName] -> TChan () -> Widget HTML GameConfig
handlerGamemasterWaitRoom waitVar waitChan = do
  lobby <- liftIO . readTVarIO $ waitVar
  let receiveUpdate = GMWaitRoomUpdate <$ (liftIO . atomically . readTChan $ waitChan)
      receiveAction =
        renderWaitRoomGM
          (renderLobby lobby)
          (GMWaitRoomKick <$> renderKick "")
          (GMWaitRoomStart <$> renderGameConfig lobby "" defaultGameConfig)
      loop = handlerGamemasterWaitRoom waitVar waitChan
  action <- receiveUpdate <|> receiveAction
  case action of
    GMWaitRoomUpdate -> loop
    GMWaitRoomKick playerName -> do
      liftIO . atomically $ do
        modifyTVar' waitVar $ \lobby -> [p | p <- lobby, p /= playerName]
        writeTChan waitChan ()
        readTChan waitChan
      loop
    GMWaitRoomStart gameConfig -> return gameConfig
