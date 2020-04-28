{-# LANGUAGE OverloadedStrings #-}

module Interface.Gamemaster.Board.Render where

import           Concur.Core              (Widget)
import           Concur.Replica           (HTML)
import qualified Concur.Replica.DOM       as H
import qualified Concur.Replica.DOM.Props as P
import           Data.List                (sort)
import           Data.Map                 (elems, toAscList)
import           Data.Text                (concat, intercalate)
import           Engine.State
import           Interface.Types          (PlayerName)
import           Lens.Micro.Platform      (each, singular, (^.), (^..))
import           Prelude                  hiding (concat)

renderBoard :: Widget HTML a -> Widget HTML a -> Widget HTML a -> Widget HTML a
renderBoard leaderSequence questHistory voteHistory = H.div [] [leaderSequence, questHistory, voteHistory]

renderLeaderSequence :: [PlayerName] -> Widget HTML a
renderLeaderSequence playerNames =
  H.div []
  [ H.div [P.className "text-muted"] [H.text "Leader Sequence"],
    H.div [] [H.text $ intercalate " > " playerNames]
  ]

renderQuestHistory :: [Phase] -> Widget HTML a
renderQuestHistory endPhases =
  H.div [P.className "table-responsive"]
    [H.table [P.className "table table-bordered"] (caption : header : body ++ [lastRow | length body == 0])]
  where
    renderRow endPhase = H.tr []
      [ H.td [] [H.text $ questText $ endPhase ^. singular endPhaseQuestResult . voted . proposed . quest],
        H.td [] [H.text $ intercalate ", " $ endPhase ^. singular endPhaseQuestResult . voted . proposed . team ^.. each . name],
        H.td [] [H.text $ concat [ if f == Success then "😇" else "😈" | f <- sort . elems $ endPhase ^. singular endPhaseQuestResult . results]]
      ]
    questText quest = concat
      [ quest ^. questName,
        " ",
        concat $ take (quest ^. teamSize) $ repeat "👤",
        if quest ^. requiredFails > 1 then "⚠️" else ""
      ]
    header = H.tr []
      [ H.th [P.style [("width", "25%")]] [H.text "Quest"],
        H.th [P.style [("width", "25%")]] [H.text "Party"],
        H.th [] [H.text "Result"]
      ]
    lastRow = H.tr [] $ take 3 $ repeat (H.td [] [H.text "-"])
    body = map renderRow endPhases
    caption = H.caption [P.style [("caption-side", "top"), ("text-align", "center")]] [H.text "Quest History"]

renderVoteHistory :: [PlayerName] -> [Phase] -> Widget HTML a
renderVoteHistory playerNames phases =
  H.div [P.className "table-responsive"]
    [H.table [P.className "table table-bordered"] (caption : header : body ++ [lastRow | length body == 0])]
  where
    renderRow (Voted (Proposed _ team quest) votes) = H.tr [] $
      [ H.td [] [H.text $ questText quest],
        H.td [] [H.text $ intercalate ", " (team ^.. each . name)]
      ]
      ++ [H.td [] [H.text $ if vote == Approve then "✔" else "✖"] | (_, vote) <- toAscList votes]
    header = H.tr [] $
      [ H.th [P.style [("width", "25%")]] [H.text "Quest"],
        H.th [P.style [("width", "25%")]] [H.text "Party"]
      ]
      ++ [H.th [] [H.text x] | x <- sort playerNames]
    body = map renderRow $ concatMap getVotes phases
    questText quest = concat
      [ quest ^. questName,
        " ",
        concat $ take (quest ^. teamSize) $ repeat "👤",
        if quest ^. requiredFails > 1 then "⚠️" else ""
      ]
    lastRow = H.tr [] $ take (length playerNames + 2) $ repeat (H.td [] [H.text "-"])
    caption = H.caption [P.style [("caption-side", "top"), ("text-align", "center")]] [H.text "Vote History"]
    getVotes phase = case phase of
        EndPhase {}       -> (phase ^. singular endPhaseRejected) ++ [phase ^. singular endPhaseQuestResult . voted]
        QuestPhase {}     -> (phase ^. singular questPhaseRejected) ++ [phase ^. singular questPhaseApproved]
        VotingPhase {}    -> phase ^. singular votingPhaseRejected
        SelectionPhase {} -> phase ^. singular selectionPhaseRejected
        _                 -> []
