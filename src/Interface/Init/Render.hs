{-# LANGUAGE OverloadedStrings #-}

module Interface.Init.Render where

import           Concur.Core               (Widget)
import           Concur.Replica            (HTML)
import qualified Concur.Replica.DOM        as H
import qualified Concur.Replica.DOM.Events as E
import qualified Concur.Replica.DOM.Props  as P
import           Data.Text                 (Text, all, length, null, toLower)
import           Interface.Types           (PlayerName, RoomId)
import           Prelude                   hiding (all, length, null)

data InitWidgetAction = InitWidgetRoomId RoomId | InitWidgetPlayerName PlayerName | InitWidgetJoin

renderInit :: Text -> RoomId -> PlayerName -> Widget HTML (RoomId, PlayerName)
renderInit message roomId playerName = do
  action <-
    H.div
      [P.className "window text-center"]
      [ H.div
          [P.className "container", P.style [("margin-top", "40px")]]
          [ InitWidgetRoomId
              <$> H.input [P.className "container text-center", P.placeholder "Enter room name", P.value roomId, E.targetValue . E.target <$> E.onChange],
            InitWidgetPlayerName
              <$> H.input [P.className "container text-center", P.placeholder "Enter player name", P.value playerName, E.targetValue . E.target <$> E.onChange],
            InitWidgetJoin <$ H.button [E.onClick, P.className "btn btn-outline-primary"] [H.text "Join Room"],
            H.p [P.className "text-danger"] [H.text message]
          ]
      ]
  let isNonEmpty x = not $ null x
      isAlphanumeric = all (`elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890" :: String))
      isMax10Char x = length x <= 10
      isValid x = isNonEmpty x && isAlphanumeric x && isMax10Char x
  case action of
    InitWidgetRoomId r -> renderInit message r playerName
    InitWidgetPlayerName p -> renderInit message roomId p
    InitWidgetJoin ->
      if isValid roomId && isValid playerName
        then return (toLower roomId, toLower playerName)
        else renderInit "Inputs must be alphanumeric and only up to 10 characters!" roomId playerName
