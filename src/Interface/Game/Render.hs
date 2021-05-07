{-# LANGUAGE OverloadedStrings #-}

module Interface.Game.Render where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import Data.List (sort)
import Data.Text (intercalate)
import Engine.Helpers (isGood)
import Engine.State
import Fmt ((+|), (+||), (|+), (||+))
import Interface.Bulma as Bulma
import Lens.Micro.Platform ((^.))

renderGame :: Widget HTML a -> Widget HTML a -> Widget HTML a -> Widget HTML a
renderGame gameCharInfo gameCore gameBoard =
  H.div
    []
    [ H.div [Bulma.block] [gameCharInfo],
      H.div [Bulma.block] [H.div [Bulma.containerBox] [gameCore]],
      H.div [Bulma.block] [gameBoard]
    ]

renderCharInfo :: Player -> [Player] -> Widget HTML a
renderCharInfo self allPlayers =
  H.div [] [H.text $ "You are " +|| role ||+ ". " +| roleInfo role]
  where
    role = self ^. character
    roleInfo role = case role of
      Merlin ->
        "You see "
          +| intercalate ", " [p ^. name | p <- sort allPlayers, not $ isGood p, p ^. character /= Mordred]
          |+ " as evil."
      Percival ->
        "One of "
          +| intercalate ", " [p ^. name | p <- sort allPlayers, p ^. character `elem` [Merlin, Morgana]]
          |+ " is Merlin while the other is Morgana."
      MinionOfMordred ->
        "Your allies are "
          +| intercalate ", " [p ^. name | p <- sort allPlayers, not $ isGood p, p ^. character /= Oberon, p /= self]
          |+ "."
      LoyalServantOfArthur -> "You know nothing. Yay."
      Mordred -> roleInfo MinionOfMordred
      Assassin -> roleInfo MinionOfMordred
      Morgana -> roleInfo MinionOfMordred
      Oberon -> "You have no friends. Sad."
