{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Init.Render where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as E
import qualified Concur.Replica.DOM.Props as P
import Data.Text (Text, all, length, null, toLower)
import Interface.Bulma as Bulma
import Interface.Types (PlayerName, RoomId)
import Prelude hiding (all, length, null)

data InitWidgetAction = InitWidgetRoomId RoomId | InitWidgetPlayerName PlayerName | InitWidgetJoin

renderInit :: Text -> RoomId -> PlayerName -> Widget HTML (RoomId, PlayerName)
renderInit message roomId playerName = do
  action <-
    H.div
      [Bulma.containerBoxCentered]
      [ H.header [Bulma.title] [H.text "Avalon"],
        H.div
          [Bulma.block]
          [ InitWidgetRoomId <$> inputWidget "Enter room name" roomId,
            InitWidgetPlayerName <$> inputWidget "Enter player name" playerName
          ],
        InitWidgetJoin
          <$ H.button
            [E.onClick, Bulma.button]
            [H.text "Join Room"]
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

inputWidget :: (H.WidgetConstraints m) => Text -> Text -> m Text
inputWidget placeholder value =
  H.input
    [ P.placeholder placeholder,
      P.value value,
      E.targetValue . E.target <$> E.onChange,
      Bulma.input
    ]
