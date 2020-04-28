module Interface.Game.Handler where

import Concur.Core                      (Widget)
import Concur.Replica                   (HTML)
import Control.Concurrent.STM.TChan     (TChan)
import Control.Concurrent.STM.TVar      (TVar, readTVarIO)
import Control.Monad.IO.Class          (liftIO)
import Engine.State
import Interface.Game.Core.Handler      (handlerGameCore)
import Interface.Game.Render            (renderGame, renderCharInfo)
import Interface.Types                  (PlayerName)
import Lens.Micro.Platform             ((^.))

handlerGame :: PlayerName -> TVar Game -> TChan () -> Widget HTML a
handlerGame playerName gameVar gameChan = do
  game <- liftIO . readTVarIO $ gameVar
  let allPlayers = game ^. players
      self = head [p | p <- allPlayers, p ^. name == playerName]
  renderGame
    (renderCharInfo self allPlayers)
    (handlerGameCore playerName gameVar gameChan)