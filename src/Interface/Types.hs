{-# LANGUAGE TemplateHaskell #-}

module Interface.Types where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar (TVar)
import Data.Map (Map)
import Data.Text (Text)
import Engine.State (Game)
import Lens.Micro.Platform (makeLenses)

type RoomId = Text

type PlayerName = Text

data Room
  = GameRoom
      { _roomGameState :: TVar Game,
        _roomGameChan :: TChan ()
      }
  | WaitingRoom
      { _roomLobby :: TVar [PlayerName],
        _roomLobbyChan :: TChan ()
      }

makeLenses ''Room

data RoomMeta = RoomMeta
  { _roomState :: TVar Room,
    _roomChannel :: TChan ()
  }

makeLenses ''RoomMeta

type Server = Map RoomId RoomMeta

data Action a = Action | Update | ActionValue a
