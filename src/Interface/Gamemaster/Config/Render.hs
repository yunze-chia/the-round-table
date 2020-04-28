{-# LANGUAGE OverloadedStrings #-}

module Interface.Gamemaster.Config.Render where

import           Concur.Core               (Widget)
import           Concur.Replica            (HTML)
import qualified Concur.Replica.DOM        as H
import qualified Concur.Replica.DOM.Events as E
import qualified Concur.Replica.DOM.Props  as P
import           Data.Map                  ((!))
import           Data.Text                 (Text, pack)
import           Engine.Helpers            (evilRequired)
import           Engine.State
import           Fmt                       ((+||), (||+))
import           Interface.Types           (Action (Action, ActionValue), PlayerName)
import           Lens.Micro.Platform       ((%~), (&), (.~), (^.))

renderGameConfig :: [PlayerName] -> Text -> GameConfig -> Widget HTML GameConfig
renderGameConfig playerNames message gameConfig = do
  action <-
    H.div
      []
      [ H.h6 [] [H.text "Game Options"],
        H.div
          [P.className "form-group row"]
          [ H.div [P.className "col-6 text-right"] [H.text "Special Characters"],
            H.div
              [P.className "col-6 text-left"]
              [ ActionValue x
                  <$ H.div
                    [P.className "form-check"]
                    [ H.input
                        [ P.type_ "checkbox",
                          P.id $ "select-" +|| x ||+ "",
                          E.onChange,
                          P.checked $ x `elem` gameConfig ^. characters,
                          P.className "form-check-input",
                          P.disabled $ x `elem` [Merlin, Assassin]
                        ],
                      H.label [P.for $ "select-" +|| x ||+ "", P.className "form-check-label"] [H.text $ pack . show $ x]
                    ]
                | x <- [Merlin, Assassin, Percival, Morgana, Mordred, Oberon]
              ]
          ],
        H.div
          [P.className "form-group row"]
          [ H.div [P.className "col-6 text-right"] [H.text "Quest Targeting"],
            H.div
              [P.className "col-6 text-left"]
              [ H.div
                  [P.className "form-check"]
                  [ H.input
                      [ P.type_ "checkbox",
                        P.className "form-check-input",
                        P.disabled True
                      ]
                  ]
              ]
          ],
        H.div
          [P.className "form-group row"]
          [ H.div [P.className "col-6 text-right"] [H.text "Lady Of The Lake"],
            H.div
              [P.className "col-6 text-left"]
              [ H.div
                  [P.className "form-check"]
                  [ H.input
                      [ P.type_ "checkbox",
                        P.className "form-check-input",
                        P.disabled True
                      ]
                  ]
              ]
          ],
        H.div
          [P.className "form-group row"]
          [ H.label [P.className "col-6 text-right"] [H.label [P.for "lancelot", P.className "col-form-label"] [H.text "Lancelot"]],
            H.div
              [P.className "col-3"]
              [ H.select
                  [ P.value "None",
                    P.className "form-control",
                    P.id "lancelot",
                    P.disabled True
                  ]
                  [H.option [P.value x] [H.text x] | x <- ["None", "Variant 1", "Variant 2", "Variant 3"]]
              ]
          ],
        Action <$ H.button [E.onClick, P.className "btn btn-outline-primary"] [H.text "Start Game"],
        H.div [] [H.small [] [H.text "Start only when everyone is present."]],
        H.div [P.className "text-danger"] [H.text message]
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
renderReturn = H.div [] [() <$ H.button [E.onClick, P.className "btn btn-outline-primary"] [H.text "Return to Lobby"]]

renderKick :: PlayerName -> Widget HTML PlayerName
renderKick playerName = do
  action <-
    H.div
      [P.className "input-group justify-content-center"]
      [ ActionValue
          <$> H.input
            [ E.targetValue . E.target <$> E.onChange,
              P.placeholder "Enter player name",
              P.value playerName
            ],
        H.div [P.className "input-group-append"] [Action <$ H.button [E.onClick, P.className "btn btn-outline-primary"] [H.text "Kick"]]
      ]
  case action of
    ActionValue p -> renderKick p
    Action        -> return playerName
