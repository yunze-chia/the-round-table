{-# LANGUAGE TemplateHaskell #-}

module Engine.State where

import Data.Map            (Map)
import Data.Text           (Text)
import Lens.Micro.Platform (makeLenses)

data Character
  = Merlin
  | Assassin
  | Percival
  | Morgana
  | Mordred
  | Oberon
  | LoyalServantOfArthur
  | MinionOfMordred
  | GoodLancelot
  | BadLancelot
  deriving (Show, Eq, Ord)

data VoteToken = Approve | Reject deriving (Show, Eq)

data QuestToken = Success | Fail deriving (Show, Eq, Ord)

data Loyalty = Good | Evil deriving (Show)

data LoyaltyCards = Switch | NoChange deriving (Show)

data Player = Player { _name      :: Text
                     , _character :: Character
                     }
    deriving (Show, Eq, Ord)

makeLenses ''Player

data Quest = Quest { _questName     :: Text
                   , _teamSize      :: Int
                   , _requiredFails :: Int
                   }
    deriving (Show, Eq)

makeLenses ''Quest

data Proposed = Proposed { _leader :: Player
                         , _team   :: [Player]
                         , _quest  :: Quest
                         }
    deriving (Show)

makeLenses ''Proposed

data Voted = Voted { _proposed :: Proposed
                   , _votes    :: Map Player VoteToken
                   }
    deriving (Show)

makeLenses ''Voted

data QuestResult = QuestResult { _voted   :: Voted
                               , _results :: Map Player QuestToken
                               }
    deriving (Show)

makeLenses ''QuestResult

data Phase = StartPhase
           | SelectionPhase { _selectionPhaseRejected :: [Voted]
                            , _selectionPhaseLeader   :: Player
                            }
           | VotingPhase { _votingPhaseRejected :: [Voted]
                         , _votingPhaseProposed :: Proposed
                         , _votingPhaseVotes    :: Map Player VoteToken
                         }
           | QuestPhase { _questPhaseRejected :: [Voted]
                        , _questPhaseApproved :: Voted
                        , _questPhaseOutcomes :: Map Player QuestToken
                        }
           | EndPhase { _endPhaseRejected    :: [Voted]
                      , _endPhaseQuestResult :: QuestResult
                      }
           | AssassinationPhase
           | PostAssassination { _assassinated :: Player
                               }
           | GameOver { _winner       :: Loyalty
                      , _winCondition :: Text
                      }
    deriving (Show)

makeLenses ''Phase

-- replace with template haskell
isStartPhase :: Phase -> Bool
isStartPhase StartPhase {} = True
isStartPhase _             = False

isSelectionPhase :: Phase -> Bool
isSelectionPhase SelectionPhase {} = True
isSelectionPhase _                 = False

isVotingPhase :: Phase -> Bool
isVotingPhase VotingPhase {} = True
isVotingPhase _              = False

isQuestPhase :: Phase -> Bool
isQuestPhase QuestPhase {} = True
isQuestPhase _             = False

isEndPhase :: Phase -> Bool
isEndPhase EndPhase {} = True
isEndPhase _           = False

isAssassinationPhase :: Phase -> Bool
isAssassinationPhase AssassinationPhase {} = True
isAssassinationPhase _                     = False

isPostAssassination :: Phase -> Bool
isPostAssassination PostAssassination {} = True
isPostAssassination _                    = False

isGameOver :: Phase -> Bool
isGameOver GameOver {} = True
isGameOver _           = False

data LancelotVariant = NoLancelot | Variant1 | Variant2 | Variant3

data GameConfig = GameConfig { _characters      :: [Character]
                             , _questTargeting  :: Bool
                             , _ladyOfTheLake   :: Bool
                             , _lancelotVariant :: LancelotVariant
                             }

makeLenses ''GameConfig

data Game = Game { _players   :: [Player]
                 , _present   :: Phase
                 , _history   :: [Phase]
                 , _future    :: [Quest]
                 }
    deriving (Show)

makeLenses ''Game