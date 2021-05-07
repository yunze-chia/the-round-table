{-# LANGUAGE OverloadedStrings #-}

module Engine.Game where

import Data.Map (elems, empty, fromList, insert, size, (!))
import Data.Text (Text)
import Engine.Helpers
import Engine.State
import Lens.Micro.Platform ((%~), (&), (.~), (^.))
import System.Random (RandomGen)

newGame :: RandomGen g => g -> GameConfig -> [Text] -> Game
newGame randomGen (GameConfig characters _ _ _) playerNames =
  Game
    { _players = players,
      _present = StartPhase,
      _history = [],
      _future = futureQuests
    }
  where
    players = shuffle randomGen $ zipWith Player playerNames $ shuffle randomGen characters
    futureQuests = map (\(x, y, z) -> Quest x y z) (questRequiredNumber ! length players)

resolveStartPhase :: Game -> Game
resolveStartPhase game = game & present .~ SelectionPhase [] leader
  where
    leader = head $ game ^. players

resolveSelectionPhase :: [Player] -> Quest -> Game -> Game
resolveSelectionPhase team quest game =
  if voteNumber < 5
    then game & present .~ newVotingPhase
    else
      resolveVotingPhase' $
        game & present .~ newVotingPhase
          & present . votingPhaseVotes .~ allApprove
  where
    voteNumber = length rejects + 1
    SelectionPhase rejects leader = game ^. present
    newVotingPhase = VotingPhase rejects newProposed empty
    newProposed = Proposed leader team quest
    allApprove = fromList $ zip (game ^. players) (repeat Approve)

resolveVotingPhase' :: Game -> Game
resolveVotingPhase' game =
  if approveCount > rejectCount
    then
      game & present .~ newQuestPhase
        & future .~ updatedFutureQuests
    else
      game & players %~ rotateLeader
        & present .~ newSelectionPhase
  where
    approveCount = length [x | x <- elems votes, x == Approve]
    rejectCount = length (elems votes) - approveCount
    newQuestPhase = QuestPhase rejects newVoted empty
    VotingPhase rejects proposed votes = game ^. present
    newVoted = Voted proposed votes
    rotateLeader (x : xs) = xs ++ [x]
    newSelectionPhase = SelectionPhase (rejects ++ [newVoted]) newLeader
    (_ : newLeader : _) = game ^. players
    Proposed _ _ quest = proposed
    updatedFutureQuests = [x | x <- game ^. future, x /= quest]

resolveVotingPhase :: Player -> VoteToken -> Game -> Game
resolveVotingPhase player vote game =
  if numVotes == numPlayers
    then resolveVotingPhase' updatedGame
    else updatedGame
  where
    numVotes = size (updatedGame ^. present . votingPhaseVotes)
    numPlayers = length $ game ^. players
    updatedGame = game & present . votingPhaseVotes %~ insert player vote

resolveQuestPhase' :: Game -> Game
resolveQuestPhase' game
  | questOutcome == Fail && pastReds == 2 =
    game & history %~ appendToHistory
      & present .~ GameOver {_winner = Evil, _winCondition = "3 quests failed!"}
  | questOutcome == Success && pastBlues == 2 =
    game & history %~ appendToHistory
      & present .~ AssassinationPhase
  | otherwise =
    game & present .~ newEndPhase
  where
    QuestPhase rejects approved outcomes = game ^. present
    questOutcome = getQuestOutcome newEndPhase
    pastOutcomes = map getQuestOutcome (game ^. history)
    pastReds = length [x | x <- pastOutcomes, x == Fail]
    pastBlues = length (game ^. history) - pastReds
    appendToHistory x = x ++ [newEndPhase]
    newEndPhase = EndPhase rejects (QuestResult approved outcomes)

resolveQuestPhase :: Player -> QuestToken -> Game -> Game
resolveQuestPhase player outcome game =
  if numOutcomes == partySize
    then resolveQuestPhase' updatedGame
    else updatedGame
  where
    newOutcomes = insert player outcome $ game ^. present . questPhaseOutcomes
    numOutcomes = size newOutcomes
    partySize = length $ game ^. present . questPhaseApproved . proposed . team
    updatedGame = game & present . questPhaseOutcomes .~ newOutcomes

resolveEndPhase :: Game -> Game
resolveEndPhase game =
  game & history %~ appendToHistory
    & players %~ rotateLeader
    & present .~ StartPhase
  where
    appendToHistory x = x ++ [game ^. present]
    rotateLeader (x : xs) = xs ++ [x]

resolveAssassinationPhase :: Player -> Game -> Game
resolveAssassinationPhase killed game =
  if killed ^. character == Merlin
    then
      game & history %~ appendToHistory
        & present .~ GameOver {_winner = Evil, _winCondition = "Merlin died. Meh."}
    else
      game & history %~ appendToHistory
        & present .~ GameOver {_winner = Good, _winCondition = "Merlin survives!!!"}
  where
    appendToHistory x = x ++ [PostAssassination {_assassinated = killed}]
