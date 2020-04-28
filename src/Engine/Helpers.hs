{-# LANGUAGE OverloadedStrings #-}

module Engine.Helpers where

import Data.Map            (Map, elems, fromList)
import Data.Text           (Text)
import Engine.State
import Lens.Micro.Platform ((^.))
import System.Random       (RandomGen, randomR)

defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig [Merlin, Assassin] False False NoLancelot

-- Map playerCount [(questTeamCount, failCount)]
questRequiredNumber :: Map Int [(Text, Int, Int)]
questRequiredNumber =
  fromList
    [ (5, zip3 questNames [2, 3, 2, 3, 3] [1, 1, 1, 1, 1]),
      (6, zip3 questNames [2, 3, 4, 3, 4] [1, 1, 1, 1, 1]),
      (7, zip3 questNames [2, 3, 3, 4, 4] [1, 1, 1, 2, 1]),
      (8, zip3 questNames [3, 4, 4, 5, 5] [1, 1, 1, 2, 1]),
      (9, zip3 questNames [3, 4, 4, 5, 5] [1, 1, 1, 2, 1]),
      (10, zip3 questNames [3, 4, 4, 5, 5] [1, 1, 1, 2, 1])
    ]

evilRequired :: Map Int Int
evilRequired =
  fromList
    [ (5, 2),
      (6, 2),
      (7, 3),
      (8, 3),
      (9, 3),
      (10, 4)
    ]

questNames :: [Text]
questNames =
  [ "#1",
    "#2",
    "#3",
    "#4",
    "#5"
  ]

getQuestOutcome :: Phase -> QuestToken
getQuestOutcome (EndPhase _ (QuestResult approved results)) =
  if fails >= required
    then Fail
    else Success
  where
    required = approved ^. (proposed . quest . requiredFails)
    fails = length [x | x <- elems results, x == Fail]

isGood :: Player -> Bool
isGood player = player ^. character `elem` [Merlin, Percival, LoyalServantOfArthur, GoodLancelot]

shuffle :: RandomGen g => g -> [a] -> [a]
-- Durstenfeld's version of Fisher-Yates shuffle (https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm)
shuffle randomGen deck = snd $ swapRecursive randomGen (deck, [])
  where
    swapRecursive _ ([], endList) = ([], endList)
    swapRecursive randomGen (initList, endList) =
      swapRecursive newRandomGen (init $ swapWithLast n initList, (initList !! n) : endList)
      where
        (n, newRandomGen) = randomR (0, length initList - 1) randomGen
    swapWithLast n list
      | n >= length list = list
      | otherwise = replaceAt n (last list) (reverse $ (list !! n) : (tail . reverse $ list))
    replaceAt n elem list
      | n >= length list = reverse $ elem : (tail . reverse $ list)
      | otherwise = take n list ++ elem : (tail . snd $ splitAt n list)
