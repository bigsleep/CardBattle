{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Battle.Types
    ( Player(..)
    , Target(..)
    , PropertySet(..)
    , PropertyTag(..)
    , Action(..)
    , TargetCapacity(..)
    , Skill(..)
    , Card(..)
    , CardState(..)
    , BattleEffect(..)
    , BattleSetting(..)
    , BattleState(..)
    , BattleCommand(..)
    , PlayerCommand(..)
    , ActionChoice(..)
    , CommandChoice(..)
    , ActionResult(..)
    , BattleLog(..)
    , BattleCommandLog(..)
    , playerAccessor
    , propertyAccessor
    , opponentPlayer
    , onTarget
    , BattleTurn

    -- generated by Lens
    , maxHp
    , maxMp
    , attack
    , defense
    , speed
    , magic
    , action
    , actions
    , target
    , targets
    , name
    , properties
    , property
    , skills
    , hp
    , mp
    , factor
    , first
    , second
    , maxTurn
    , oneTurnEffects
    , effects
    , turn
    , cardIndex
    , skillIndex
    , targetIndex
    , player
    , card
    , HasFirst
    , HasSecond
    ) where

import Control.Lens hiding (Action)
import Control.Monad (mzero)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans.RWS (RWS)
import qualified Data.Aeson as DA (Value(..), FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Attoparsec.Number as N (Number(I))
import Data.ByteString.Char8 as BS (ByteString)

class (Enum a, Bounded a) => EnumJSON a

data Player = FirstPlayer | SecondPlayer deriving (Show, Eq, Enum, Bounded)
instance EnumJSON Player

data Target =
    TargetAll |
    TargetTeam Player |
    TargetCard Player Int deriving (Show, Eq)
$(deriveJSON id ''Target)

data PropertySet = PropertySet {
    _propertysetMaxHp :: Int,
    _propertysetMaxMp :: Int,
    _propertysetAttack :: Int,
    _propertysetDefense :: Int,
    _propertysetSpeed :: Int,
    _propertysetMagic :: Int
    } deriving (Show, Eq)
$(makeFields ''PropertySet)
$(deriveJSON (drop (length "_propertyset")) ''PropertySet)

data PropertyTag =
    MaxHpTag |
    MaxMpTag |
    AttackTag |
    DefenseTag |
    SpeedTag |
    MagicTag deriving (Show, Eq, Ord, Enum, Bounded)
instance EnumJSON PropertyTag

data Action =
    Defense Int |
    Attack Int |
    Heal Int Int |
    Buff PropertyTag Int Int Int
    deriving (Show, Eq, Ord)
$(deriveJSON id ''Action)

data TargetCapacity =
    TargetCapacityOne |
    TargetCapacityTeam |
    TargetCapacityAll |
    TargetCapacityAlmighty |
    TargetCapacityOwn |
    TargetCapacityOpponent |
    TargetCapacitySelf |
    TargetCapacityAlive |
    TargetCapacityDead |
    TargetCapacityMixAnd [TargetCapacity] |
    TargetCapacityMixOr [TargetCapacity]
    deriving (Show, Eq)
$(deriveJSON id ''TargetCapacity)

data Skill = Skill {
    _skillAction :: Action,
    _skillTarget :: TargetCapacity
    } deriving (Show, Eq)
$(makeFields ''Skill)
$(deriveJSON (drop (length "_skill")) ''Skill)

data Card = Card {
    _cardName :: BS.ByteString,
    _cardProperties :: PropertySet,
    _cardSkills :: [Skill]
    } deriving (Show, Eq)
$(makeFields ''Card)
$(deriveJSON (drop (length "_card")) ''Card)

data CardState = CardState {
    _cardstateHp :: Int,
    _cardstateMp :: Int
} deriving (Show, Eq)
$(makeFields ''CardState)
$(deriveJSON (drop (length "_cardstate")) ''CardState)

data BattleEffect = BattleEffect {
    _battleeffectTarget :: (Player, Int),
    _battleeffectProperty :: PropertyTag,
    _battleeffectFactor :: Int
    } deriving (Show, Eq)
$(makeFields ''BattleEffect)
$(deriveJSON (drop (length "_battleeffect")) ''BattleEffect)

data BattleSetting = BattleSetting {
    _battlesettingFirst :: [Card],
    _battlesettingSecond :: [Card],
    _battlesettingMaxTurn :: Maybe Int
    } deriving (Show, Eq)
$(makeFields ''BattleSetting)
$(deriveJSON (drop (length "_battlesetting")) ''BattleSetting)

data BattleState = BattleState {
    _battlestateFirst :: [CardState],
    _battlestateSecond :: [CardState],
    _battlestateOneTurnEffects :: [BattleEffect],
    _battlestateEffects :: [(BattleEffect, Int)],
    _battlestateTurn :: Int
    } deriving (Show, Eq)
$(makeFields ''BattleState)
$(deriveJSON (drop (length "_battlestate")) ''BattleState)

data PlayerCommand = PlayerCommand {
    _playercommandCardIndex :: Int,
    _playercommandSkillIndex :: Int,
    _playercommandTargetIndex :: Int
    } deriving (Show, Eq)
$(makeFields ''PlayerCommand)
$(deriveJSON (drop (length "_battlecommand")) ''PlayerCommand)

data BattleCommand = BattleCommand {
    _battlecommandPlayer :: Player,
    _battlecommandCard :: Int,
    _battlecommandAction :: Action,
    _battlecommandTarget :: Target
    } deriving (Show, Eq)
$(makeFields ''BattleCommand)
$(deriveJSON (drop (length "_battlecommand")) ''BattleCommand)

data ActionChoice = ActionChoice {
    _actionchoiceSkillIndex :: Int,
    _actionchoiceAction :: Action,
    _actionchoiceTargets :: [Target]
    } deriving (Show, Eq)
$(makeFields ''ActionChoice)
$(deriveJSON (drop (length "_actionchoice")) ''ActionChoice)

data CommandChoice = CommandChoice {
    _commandchoiceCardIndex :: Int,
    _commandchoiceActions :: [ActionChoice]
    } deriving (Show, Eq)
$(makeFields ''CommandChoice)
$(deriveJSON (drop (length "_commandchoice")) ''CommandChoice)

-- log
data ActionResult =
    Consume (Player, Int) CardState |
    StateChange (Player, Int) CardState |
    PropertyChange (Player, Int) PropertyTag Int |
    Death (Player, Int) |
    FailureBecauseDeath |
    Underqualified |
    ActionFailure deriving (Show, Eq)
$(deriveJSON id ''ActionResult)

newtype EffectExpiration = EffectExpiration BattleEffect deriving (Show, Eq)
$(deriveJSON id ''EffectExpiration)

data BattleCommandLog =
    BattleCommandLog BattleCommand [ActionResult] deriving (Show, Eq)
$(deriveJSON id ''BattleCommandLog)

data BattleLog = BattleLog BattleState [BattleCommandLog] [EffectExpiration] deriving (Show, Eq)
$(deriveJSON id ''BattleLog)

-- BattleTurn
type BattleTurn = ErrorT String (RWS BattleSetting [BattleCommandLog] BattleState)

onTarget :: Player -> Int -> Target -> Bool
onTarget _ _ TargetAll = True
onTarget q _ (TargetTeam p) = p == q
onTarget q y (TargetCard p x) = p == q && x == y

playerAccessor :: (HasFirst a b, HasSecond a b)
               => Player -> Lens' a b
playerAccessor FirstPlayer = first
playerAccessor SecondPlayer = second

propertyAccessor :: (HasMaxHp a b, HasMaxMp a b, HasAttack a b, HasDefense a b, HasSpeed a b, HasMagic a b)
                 => PropertyTag -> Lens' a b
propertyAccessor MaxHpTag = maxHp
propertyAccessor MaxMpTag = maxMp
propertyAccessor AttackTag = attack
propertyAccessor DefenseTag = defense
propertyAccessor SpeedTag = speed
propertyAccessor MagicTag = magic

opponentPlayer :: Player -> Player
opponentPlayer FirstPlayer = SecondPlayer
opponentPlayer SecondPlayer = FirstPlayer

-- for Enum conversion
instance (EnumJSON a) => DA.FromJSON a where
    parseJSON (DA.Number (N.I i)) = case safeToEnum $ fromInteger i of
                                         Just e -> return e
                                         Nothing -> mzero
    parseJSON _ = mzero

instance (EnumJSON a) => DA.ToJSON a where
    toJSON e = DA.Number . N.I . toInteger . fromEnum $ e

safeToEnum :: forall t . (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i = if (i >= fromEnum (minBound :: t)) && (i <= fromEnum (maxBound :: t))
                  then Just . toEnum $ i
                  else Nothing

