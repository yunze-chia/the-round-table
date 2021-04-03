{-# LANGUAGE OverloadedStrings #-}

module Interface.Game.Config.Render where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as E
import qualified Concur.Replica.DOM.Props as P
import Data.Map ((!))
import Data.Text (Text, pack)
import Engine.Helpers (evilRequired)
import Engine.State (Character (..), GameConfig, characters)
import Fmt ((+||), (||+))
import Interface.Bulma as Bulma (containerBox, title)
import Interface.Types (Action (Action, ActionValue), PlayerName)
import Lens.Micro.Platform ((%~), (&), (.~), (^.))

renderGameConfig :: [PlayerName] -> Text -> GameConfig -> Widget HTML GameConfig
renderGameConfig playerNames message gameConfig = do
  action <-
    H.div
      [Bulma.containerBox]
      [ H.header [Bulma.title] [H.text "Game Options"],
        H.div
          []
          [ H.div [] [H.text "Special Characters"],
            H.div
              []
              [ ActionValue x
                  <$ H.div
                    []
                    [ H.input
                        [ P.type_ "checkbox",
                          P.id $ "select-" +|| x ||+ "",
                          E.onChange,
                          P.checked $ x `elem` gameConfig ^. characters,
                          P.disabled $ x `elem` [Merlin, Assassin]
                        ],
                      H.label [P.for $ "select-" +|| x ||+ ""] [H.text $ pack . show $ x]
                    ]
                | x <- [Merlin, Assassin, Percival, Morgana, Mordred, Oberon]
              ]
          ],
        Action <$ H.button [E.onClick] [H.text "Start Game"],
        H.div [] [H.small [] [H.text "Start only when everyone is present."]],
        H.div [] [H.text message]
      ]
  case action of
    ActionValue char ->
      if char `elem` gameConfig ^. characters
        then renderGameConfig playerNames message $ gameConfig & characters %~ filter (/= char)
        else renderGameConfig playerNames message $ gameConfig & characters %~ (char :)
    Action -> case renderErrorMessage playerNames (gameConfig ^. characters) of
      Nothing -> do
        let chars = gameConfig ^. characters
            evilChars = take (evilRequired ! playerCount) $ [c | c <- chars, c `elem` [Assassin, Morgana, Oberon, Mordred]] ++ repeat MinionOfMordred
            goodChars = take (playerCount - length evilChars) $ [c | c <- chars, c `elem` [Merlin, Percival]] ++ repeat LoyalServantOfArthur
            playerCount = length playerNames
        return $ gameConfig & characters .~ (evilChars ++ goodChars)
      Just errorMessage -> renderGameConfig playerNames errorMessage gameConfig
  where
    renderErrorMessage playerNames chars
      | length playerNames < 5 = Just "Game requires at least 5 players!"
      | length [c | c <- chars, c `elem` [Assassin, Morgana, Oberon, Mordred]] > evilRequired ! length playerNames = Just "Too many evil characters selected!"
      | Percival `elem` chars && (Morgana `notElem` chars || Merlin `notElem` chars) = Just "Morgana must be present to confuse Percival!"
      | otherwise = Nothing

renderReturn :: Widget HTML ()
renderReturn = H.div [] [() <$ H.button [E.onClick] [H.text "Reset room"]]
