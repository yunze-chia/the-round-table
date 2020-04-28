{-# LANGUAGE OverloadedStrings #-}

module Interface.Room.Render where

import           Concur.Core              (Widget)
import           Concur.Replica           (HTML)
import qualified Concur.Replica.DOM       as H
import qualified Concur.Replica.DOM.Props as P
import           Data.Text                (intercalate)
import           Fmt                      ((+|), (|+))
import           Interface.Types          (PlayerName, RoomId)

renderRoom :: Widget HTML a -> Widget HTML a -> Widget HTML a
renderRoom roomInfo roomMain =
  H.div
    [P.className "window text-center"]
    [ roomInfo,
      H.div [P.style [("margin-top", "1em")]] [roomMain]
    ]

renderRoomInfo :: RoomId -> PlayerName -> Widget HTML a
renderRoomInfo roomId playerName =
  H.small [] [H.text $ "You are in room " +| roomId |+ " as player " +| playerName |+ "."]

renderBounced :: Widget HTML a
renderBounced = H.div [] [H.text "Please wait for ongoing game to end to connect as new user. Refresh page to return."]

renderLobby :: [PlayerName] -> Widget HTML a
renderLobby playerNames =
  H.div
    []
    [ H.h6 [] [H.text "Waiting for more players..."],
      H.div [] [H.text "Players in lobby: "],
      H.div [] [H.text $ intercalate ", " playerNames]
    ]
