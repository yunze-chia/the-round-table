{-# LANGUAGE OverloadedStrings #-}

module Interface.Gamemaster.Render where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import Fmt ((+|), (|+))
import Interface.Bulma as Bulma
import Interface.Types (RoomId)

renderRoomInfoGM :: RoomId -> Widget HTML a
renderRoomInfoGM roomId =
  H.small [] [H.text $ "You are in room " +| roomId |+ " as the gamemaster."]

renderWaitRoomGM :: Widget HTML a -> Widget HTML a -> Widget HTML a -> Widget HTML a
renderWaitRoomGM renderLobby renderKick renderGameConfig =
  H.div
    []
    [ H.div [Bulma.block] [renderLobby],
      H.div [Bulma.block] [renderKick],
      H.div [Bulma.block] [renderGameConfig]
    ]
