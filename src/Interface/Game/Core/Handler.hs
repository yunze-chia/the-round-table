{-# LANGUAGE OverloadedStrings #-}

module Interface.Game.Core.Handler where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Engine.Game
import Engine.Helpers (isGood)
import Engine.State
import Interface.Game.Core.Render
import Interface.Types (Action (Action, ActionValue, Update), PlayerName)
import Lens.Micro.Platform (singular, (^.), _head, _last)

handlerGameCore :: PlayerName -> TVar Game -> TChan () -> Widget HTML a
handlerGameCore playerName gameVar gameChan = do
  game <- liftIO . readTVarIO $ gameVar
  let player = head [p | p <- game ^. players, p ^. name == playerName]
      receiveUpdate = Update <$ (liftIO . atomically . readTChan $ gameChan)
      loop = handlerGameCore playerName gameVar gameChan
      receiveOrMakeUpdateThenLoop isPhase f = do
        liftIO . atomically $
          readTChan gameChan `orElse` do
            let presentPhase = game ^. present
            when (isPhase presentPhase) $ do
              modifyTVar' gameVar f
              writeTChan gameChan ()
              readTChan gameChan
        loop
  case game ^. present of
    StartPhase -> receiveOrMakeUpdateThenLoop isStartPhase resolveStartPhase
    SelectionPhase _ leader ->
      if player == leader
        then do
          let receiveAction = ActionValue <$> renderSelection leader allPlayers remainingQuests "" initialProposed
              allPlayers = game ^. players
              remainingQuests = game ^. future
              initialProposed = Proposed leader [] (game ^. future . singular _head)
          action <- receiveUpdate <|> receiveAction
          case action of
            Update -> loop
            ActionValue (Proposed _ team quest) -> receiveOrMakeUpdateThenLoop isSelectionPhase $ resolveSelectionPhase team quest
        else do
          _ <- receiveUpdate <|> renderSelectionWait leader
          loop
    VotingPhase _ proposed votes -> do
      let receiveAction = ActionValue <$> renderVoting proposed votes
      action <- receiveUpdate <|> receiveAction
      case action of
        Update -> loop
        ActionValue vote -> receiveOrMakeUpdateThenLoop isVotingPhase $ resolveVotingPhase player vote
    QuestPhase _ approved outcomes ->
      if player `elem` approved ^. proposed . team
        then do
          let receiveAction = ActionValue <$> renderQuesting approved False outcomes
          action <- receiveUpdate <|> receiveAction
          case action of
            Update -> loop
            ActionValue outcome -> receiveOrMakeUpdateThenLoop isQuestPhase $ resolveQuestPhase player outcome
        else do
          _ <- receiveUpdate <|> Action <$ renderQuesting approved True outcomes
          loop
    EndPhase _ _ -> receiveOrMakeUpdateThenLoop isEndPhase resolveEndPhase
    AssassinationPhase -> do
      let assassin = head [p | p <- game ^. players, p ^. character == Assassin]
      if player == assassin
        then do
          let goodPlayers = [p | p <- game ^. players, isGood p]
              receiveAction = ActionValue <$> renderAssassination goodPlayers (head goodPlayers)
          action <- receiveUpdate <|> receiveAction
          case action of
            Update -> loop
            ActionValue assassinated -> receiveOrMakeUpdateThenLoop isAssassinationPhase $ resolveAssassinationPhase assassinated
        else do
          _ <- receiveUpdate <|> renderAssassinationWait assassin
          loop
    GameOver winner winCon -> do
      let lastPhase = game ^. history . singular _last
          killed = if isPostAssassination lastPhase then Just $ lastPhase ^. singular assassinated else Nothing
      _ <- receiveUpdate <|> renderGameOver winner winCon killed
      loop