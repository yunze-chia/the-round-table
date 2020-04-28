{-# LANGUAGE OverloadedStrings #-}

module Interface.Game.Core.Render where

import           Concur.Core               (Widget)
import           Concur.Replica            (HTML)
import qualified Concur.Replica.DOM        as H
import qualified Concur.Replica.DOM.Events as E
import qualified Concur.Replica.DOM.Props  as P
import           Data.List                 (sort)
import           Data.Map                  (Map, keys)
import           Data.Text                 (Text, intercalate, concat)
import           Engine.State
import           Interface.Types           (Action (Action, ActionValue))
import           Lens.Micro.Platform       (each, (^.), (^..))
import           Fmt                       ((+|), (|+), (+||), (||+))
import           Prelude hiding            (concat)

data SelectionWidgetAction = SelectionWidgetPlayer Text | SelectionWidgetQuest Text | SelectionWidgetPropose

renderSelection :: Player -> [Player] -> [Quest] -> Text -> Proposed -> Widget HTML Proposed
renderSelection leader players quests message proposed@(Proposed l t q) = do
  let playerNames = players ^.. each . name
      questText quest = concat 
        [ quest ^. questName,
          " ",
          concat $ take (quest ^. teamSize) $ repeat "👤",
          if quest ^. requiredFails > 1 then "⚠️" else ""
        ]
  action <- H.div []
      [ H.h5 [] [H.text "You are currently the leader. Select a team to go on a quest."],
        H.div
          [P.className "form-group row"]
          [ H.div [P.className "col-6 text-right"] [H.text "Select party:"],
            H.div [P.className "col-6 text-left"]
              [ SelectionWidgetPlayer x
                  <$ H.div
                    [ P.className "form-check"]
                    [ H.input
                        [ P.type_ "checkbox",
                          P.id $ "select-" +| x |+ "",
                          E.onChange,
                          P.checked $ x `elem` (t ^.. each . name),
                          P.className "form-check-input"
                        ],
                      H.label [P.for $ "select-" +| x |+ "", P.className "form-check-label"] [H.text x]
                    ]
                | x <- playerNames
              ]
          ],
        H.div
          [P.className "input-group"]
          [ H.div [P.className "input-group-prepend"] [H.label [P.for "quest", P.className "input-group-text"] [H.text "Quest: "]],
            SelectionWidgetQuest
              <$> H.select
                [ E.targetValue . E.target <$> E.onChange,
                  P.disabled True,
                  P.className "custom-select"
                ]
                [H.option [P.value $ x ^. questName] [H.text $ questText x] | x <- quests],
            H.div [P.className "input-group-append"] [SelectionWidgetPropose <$ H.button [E.onClick, P.className "btn btn-outline-primary"] [H.text "Let's Go🧐"]]
          ],
        H.div [P.className "text-danger"] [H.text message]
      ]
  case action of
    SelectionWidgetPlayer selectedPlayer ->
      renderSelection leader players quests message $ Proposed l (sort $ toggle t selectedPlayer') q
      where
        selectedPlayer' = head [x | x <- players, x ^. name == selectedPlayer]
        toggle arr x = if x `elem` arr then [i | i <- arr, i /= x] else x : arr
    SelectionWidgetQuest selectedQuest ->
      renderSelection leader players quests message $ Proposed l t selectedQuest'
      where
        selectedQuest' = head [x | x <- quests, x ^. questName == selectedQuest]
    SelectionWidgetPropose ->
      if length t == q ^. teamSize
        then return proposed
        else renderSelection leader players quests "Number of people selected must match with quest requirement!" proposed

renderSelectionWait :: Player -> Widget HTML a
renderSelectionWait leader = H.h5 [] [H.text $ "Waiting for leader " +| leader ^. name |+ " to select team..."]

renderVoting :: Proposed -> Map Player VoteToken -> Widget HTML VoteToken
renderVoting (Proposed leader team quest) votes =
  H.div
    []
    [ H.h5 [] [H.text "Waiting for votes..."],
      H.div [] [H.text $ "Party [ " +| intercalate ", " playerNames |+ " ]"],
      H.div [] [H.text $ "has been nominated by " +| leader ^. name |+ " to go on"],
      H.div [] [H.text $ "Quest [ " +| questText quest |+ " ]"],
      H.div []
        [ Approve <$ H.button [E.onClick, P.className "btn btn-outline-success", P.style [("width", "120px")]] [H.text "Approve👌🏻"],
          Reject <$ H.button [E.onClick, P.className "btn btn-outline-danger", P.style [("width", "120px")]] [H.text "Reject👎🏻"]
        ],
      H.small [] [H.text $ "Voted: " +| intercalate ", " (playerNamesVoted ++ ["-" | playerNamesVoted == []]) |+ ""]
    ]
  where
    playerNames = team ^.. each . name
    playerNamesVoted = keys votes ^.. each . name
    questText quest = concat 
      [ quest ^. questName,
        " ",
        concat $ take (quest ^. teamSize) $ repeat "👤",
        if quest ^. requiredFails > 1 then "⚠️" else ""
      ]

renderQuesting :: Voted -> Bool -> Map Player QuestToken -> Widget HTML QuestToken
renderQuesting (Voted (Proposed _ party quest) _) wait outcomes =
  H.div [] $
    [ H.h5 [] [H.text "Waiting for party to complete quest..."],
      H.div [] [H.text $ "Party [ " +| intercalate ", " playerNames |+ " ]"],
      H.div [] [H.text $ "Quest [ " +| questText quest |+ " ]"]
    ]
    ++ [ H.div []
          [ Success <$ H.button [E.onClick, P.className "btn btn-outline-success", P.style [("width", "120px")]] [H.text "Success🎉"],
            Fail <$ H.button [E.onClick, P.className "btn btn-outline-danger", P.style [("width", "120px")]] [H.text "Fail💔"]
          ] | not wait
        ]
    ++ [H.small [] [H.text $ "Completed: " +| intercalate ", " (playerNamesQuested ++ ["-" | playerNamesQuested == []]) |+ ""]]
  where
    playerNames = party ^.. each . name
    playerNamesQuested = keys outcomes ^.. each . name
    questText quest = concat 
      [ quest ^. questName,
        " ",
        concat $ take (quest ^. teamSize) $ repeat "👤",
        if quest ^. requiredFails > 1 then "⚠️" else ""
      ]

renderAssassination :: [Player] -> Player -> Widget HTML Player
renderAssassination players target = do
  let playerNames = players ^.. each . name
  action <-
    H.div
      []
      [ H.h5 [] [H.text "You are the assassin. Find Merlin to win."],
        H.div
          [P.className "input-group"]
          [ H.div [P.className "input-group-prepend"] [H.label [P.for "assassinate", P.className "input-group-text"] [H.text "Assassinate: "]],
            ActionValue
              <$> H.select
                [ E.targetValue . E.target <$> E.onChange,
                  P.value $ target ^. name,
                  P.className "custom-select"
                ]
                [H.option [P.value x] [H.text x] | x <- playerNames],
            H.div [P.className "input-group-append"] [Action <$ H.button [E.onClick, P.className "btn btn-outline-primary"] [H.text "Kill🗡"]]
          ]
      ]
  case action of
    ActionValue selectedPlayer ->
      renderAssassination players selectedPlayer'
      where
        selectedPlayer' = head [x | x <- players, x ^. name == selectedPlayer]
    Action -> return target

renderAssassinationWait :: Player -> Widget HTML a
renderAssassinationWait assassin = H.h5 [] [H.text $ "Waiting for assassin " +| assassin ^. name |+ " to kill..."]

renderGameOver :: Loyalty -> Text -> Maybe Player -> Widget HTML a
renderGameOver winner winCon killed = H.h5 [] [H.text gameOverText]
  where
    winText = "" +| winCon |+ " " +|| winner ||+ " team wins!"
    gameOverText = case killed of
      Just p -> "" +| p ^. name |+ " was assassinated! " +| winText |+ ""
      Nothing ->  winText
