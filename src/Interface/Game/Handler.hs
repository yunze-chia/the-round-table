module Interface.Game.Handler where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Engine.State
import Interface.Game.Board.Render (renderBoard, renderLeaderSequence, renderQuestHistory, renderVoteHistory)
import Interface.Game.Core.Handler (handlerGameCore)
import Interface.Game.Render (renderCharInfo, renderGame)
import Interface.Types (PlayerName)
import Lens.Micro.Platform (each, (^.), (^..))

handlerGame :: PlayerName -> TVar Game -> TChan () -> Widget HTML a
handlerGame playerName gameVar gameChan = do
  game <- liftIO . readTVarIO $ gameVar
  let allPlayers = game ^. players
      self = head [p | p <- allPlayers, p ^. name == playerName]
  dupedGameChan <- liftIO . atomically $ dupTChan gameChan
  renderGame
    (renderCharInfo self allPlayers)
    (handlerGameCore playerName gameVar gameChan)
    (handlerGameBoard gameVar dupedGameChan)

handlerGameBoard :: TVar Game -> TChan () -> Widget HTML a
handlerGameBoard gameVar gameChan = do
  game <- liftIO . readTVarIO $ gameVar
  let updateLoop widget = do
        _ <- widget <|> (liftIO . atomically $ readTChan gameChan)
        handlerGameBoard gameVar gameChan
  updateLoop $
    renderBoard
      (renderLeaderSequence (game ^. players ^.. each . name))
      (renderQuestHistory [p | p <- game ^. history ++ [game ^. present], isEndPhase p])
      (renderVoteHistory (game ^. players ^.. each . name) $ game ^. history ++ [game ^. present])
