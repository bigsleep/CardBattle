{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Battle.Types where

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import Data.Map (Map, lookup, findWithDefault, adjust)
import Data.Set hiding (map, filter, foldl, insert)
import Control.Lens hiding (Action)
import Control.Monad(forM, when)
import Control.Monad.State()
import Control.Monad.State.Class(get, put)
import Control.Monad.Reader(Reader())
import Control.Monad.Reader.Class(ask)
import Control.Monad.Trans.RWS(RWS)
import Control.Monad.Free()
import Control.Monad.Error(ErrorT, Error, noMsg, strMsg)

data Player = FirstPlayer | SecondPlayer deriving (Show, Eq, Enum)

data Target =
    TargetAll |
    TargetTeam Player |
    TargetCard Player Int deriving (Show, Eq)

data PropertySet = PropertySet {
    _propertysetMaxHp :: Int,
    _propertysetMaxMp :: Int,
    _propertysetAttack :: Int,
    _propertysetDefense :: Int,
    _propertysetSpeed :: Int,
    _propertysetMagic :: Int
    } deriving (Show, Eq)

data PropertyFactor = PropertyFactor {
    _propertyfactorMaxHp :: Double,
    _propertyfactorMaxMp :: Double,
    _propertyfactorAttack :: Double,
    _propertyfactorDefense :: Double,
    _propertyfactorSpeed :: Double,
    _propertyfactorMagic :: Double
    } deriving (Show, Eq)
    
data PropertyTag =
    MaxHpTag |
    MaxMpTag |
    AttackTag |
    DefenseTag |
    SpeedTag |
    MagicTag deriving (Show, Eq, Ord, Enum)

data Action =
    Defense |
    Attack |
    Heal Int Int |
    Buff PropertyTag Double Int Int
    deriving (Show, Eq, Ord)

data TargetCapacity =
    TargetCapacityOne |
    TargetCapacityTeam |
    TargetCapacityAll |
    TargetCapacityAlmighty |
    TargetCapacityOwn |
    TargetCapacityOpponent |
    TargetCapacitySelf |
    TargetCapacityMixAnd TargetCapacity TargetCapacity |
    TargetCapacityMixOr TargetCapacity TargetCapacity
    deriving (Show, Eq)

data Skill = Skill {
    _skillAction :: Action,
    _skillTarget :: TargetCapacity
    } deriving (Show, Eq)

data Card = Card {
    _cardName :: String,
    _cardProperties :: PropertySet,
    _cardSkills :: [Skill]
    } deriving (Show, Eq)

data CardState = CardState {
    _cardstateHp :: Int,
    _cardstateMp :: Int
} deriving (Show, Eq)

data BattleEffect = BattleEffect {
    _battleeffectAction :: Action,
    _battleeffectTarget :: Target,
    _battleeffectFactor :: PropertyFactor,
    _battleeffectRemaining :: Maybe Int
    } deriving (Show, Eq)

data BattleSetting = BattleSetting {
    _battlesettingFirst :: [Card],
    _battlesettingSecond :: [Card],
    _battlesettingMaxTurn :: Maybe Int
    } deriving (Show, Eq)

data BattleState = BattleState {
    _battlestateFirst :: [CardState],
    _battlestateSecond :: [CardState],
    _battlestateOneTurnEffects :: [BattleEffect],
    _battlestateEffects :: [BattleEffect],
    _battlestateTurn :: Int
    } deriving (Show, Eq)

data PlayerCommand = PlayerCommand {
    _playercommandCardIndex :: Int,
    _playercommandSkillIndex :: Int,
    _playercommandTargetIndex :: Int
    } deriving (Show, Eq)

data BattleCommand = BattleCommand {
    _battlecommandPlayer :: Player,
    _battlecommandCard :: Int,
    _battlecommandAction :: Action,
    _battlecommandTarget :: Target
    } deriving (Show, Eq)

data CommandChoice = CommandChoice {
    _commandchoiceCardIndex :: Int,
    _commandchoiceActions :: [ActionChoice]
    } deriving (Show, Eq)

data ActionChoice = ActionChoice {
    _actionchoiceSkillIndex :: Int,
    _actionchoiceAction :: Action,
    _actionchoiceTargets :: [Target]
    } deriving (Show, Eq)

-- Lenses
$(makeFields ''PropertySet)
$(makeFields ''PropertyFactor)
$(makeFields ''Card)
$(makeFields ''CardState)
$(makeFields ''BattleSetting)
$(makeFields ''BattleState)
$(makeFields ''PlayerCommand)
$(makeFields ''BattleEffect)
$(makeFields ''BattleCommand)
$(makeFields ''CommandChoice)
$(makeFields ''ActionChoice)
$(makeFields ''Skill)

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

type BattleTurn = ErrorT String (RWS BattleSetting [BattleCommandLog] BattleState)

type Targetable = Reader ((BattleSetting, BattleState, Player, Int), Target) (Bool)

-- log
data ActionResult =
    Consume (Player, Int) CardState |
    StateChange (Player, Int) CardState |
    PropertyChange (Player, Int) PropertySet |
    Underqualified |
    ActionFailure deriving (Show, Eq)

newtype EffectExpiration = EffectExpiration BattleEffect deriving (Show, Eq)

data BattleLog = BattleLog BattleState [BattleCommandLog] [EffectExpiration] deriving (Show, Eq)

data BattleCommandLog =
    BattleCommandLog BattleCommand [ActionResult] deriving (Show, Eq)
