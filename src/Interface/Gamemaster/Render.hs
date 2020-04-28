{-# LANGUAGE OverloadedStrings #-}

module Interface.Gamemaster.Render where

import           Concur.Core              (Widget)
import           Concur.Replica           (HTML)
import qualified Concur.Replica.DOM       as H
import qualified Concur.Replica.DOM.Props as P
import           Fmt                      ((+|), (|+))
import           Interface.Types          (RoomId)

renderRoomInfoGM :: RoomId -> Widget HTML a
renderRoomInfoGM roomId =
  H.small [] [H.text $ "You are in room " +| roomId |+ " as the gamemaster."]

renderWaitRoomGM :: Widget HTML a -> Widget HTML a -> Widget HTML a -> Widget HTML a
renderWaitRoomGM renderLobby renderKick renderGameConfig =
  H.div
    []
    [ renderLobby,
      H.div [P.style [("margin-top", "1em")]] [renderKick],
      H.div [P.style [("margin-top", "1em")]] [renderGameConfig]
    ]
