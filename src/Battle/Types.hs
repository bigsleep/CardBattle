{-# LANGUAGE TemplateHaskell #-}

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
    _maxHp :: Int,
    _maxMp :: Int,
    _attack :: Int,
    _defense :: Int,
    _speed :: Int,
    _magic :: Int
    } deriving (Show, Eq)

data PropertyFactor = PropertyFactor {
    _maxHpFactor :: Double,
    _maxMpFactor :: Double,
    _attackFactor :: Double,
    _defenseFactor :: Double,
    _speedFactor :: Double,
    _magicFactor :: Double
    } deriving (Show, Eq)
    
data PropertyFactorTag =
    MaxHpFactor |
    MaxMpFactor |
    AttackFactor |
    DefenseFactor |
    SpeedFactor |
    MagicFactor deriving (Show, Eq, Ord, Enum)

data Action =
    Defense |
    Attack |
    Heal Int Int |
    Boost PropertyFactorTag Double |
    Multi [Action]
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

data Skill = Skill Action TargetCapacity deriving (Show, Eq)

data Card = Card {
    _cardName :: String,
    _properties :: PropertySet,
    _skills :: [Skill]
    } deriving (Show, Eq)

data CardState = CardState {
    _hp :: Int,
    _mp :: Int
} deriving (Show, Eq)

data BattleEffect = BattleEffect {
    _effectAction :: Action,
    _effectTarget :: Target,
    _factor :: PropertyFactor,
    _remaining :: Maybe Int
    } deriving (Show, Eq)

data BattleSetting = BattleSetting {
    _firstCards :: [Card],
    _secondCards :: [Card],
    _maxTurn :: Maybe Int
    } deriving (Show, Eq)

data BattleState = BattleState {
    _first :: [CardState],
    _second :: [CardState],
    _effects :: [BattleEffect],
    _remainingTurn :: Maybe Int
    } deriving (Show, Eq)

data PlayerCommand = PlayerCommand {
    _cardIndex :: Int,
    _skillIndex :: Int,
    _targetIndex :: Int
    } deriving (Show, Eq)

data BattleCommand = BattleCommand {
    _player :: Player,
    _card :: Int,
    _action :: Action,
    _target :: Target
    } deriving (Show, Eq)

-- Lenses
$(makeLenses ''PropertySet)
$(makeLenses ''PropertyFactor)
$(makeLenses ''Action)
$(makeLenses ''Card)
$(makeLenses ''CardState)
$(makeLenses ''BattleSetting)
$(makeLenses ''BattleState)
$(makeLenses ''PlayerCommand)
$(makeLenses ''BattleEffect)
$(makeLenses ''BattleCommand)

playerAccessor :: Player -> Lens' BattleSetting [Card]
playerAccessor FirstPlayer = firstCards
playerAccessor SecondPlayer = secondCards

playerStateAccessor :: Player -> Lens' BattleState [CardState]
playerStateAccessor FirstPlayer = first
playerStateAccessor SecondPlayer = second

propertyFactorAccessor :: PropertyFactorTag -> Lens' PropertyFactor Double
propertyFactorAccessor MaxHpFactor = maxHpFactor
propertyFactorAccessor MaxMpFactor = maxMpFactor
propertyFactorAccessor AttackFactor = attackFactor
propertyFactorAccessor DefenseFactor = defenseFactor
propertyFactorAccessor SpeedFactor = speedFactor
propertyFactorAccessor MagicFactor = magicFactor

opponentPlayer :: Player -> Player
opponentPlayer FirstPlayer = SecondPlayer
opponentPlayer SecondPlayer = FirstPlayer

type BattleTurn = ErrorT String (RWS BattleSetting [BattleCommandLog] BattleState)

type Targetable = Reader ((BattleSetting, BattleState, Player, Int), Target) (Bool)

-- log
data ActionResult =
    Consume (Player, Int) CardState |
    StateChange (Player, Int) CardState |
    PropertyChange (Player, Int) PropertySet deriving (Show, Eq)

newtype EffectExpiration = EffectExpiration BattleEffect deriving (Show, Eq)

data BattleLog =
    StateSnapshot BattleState |
    TurnLog [BattleCommandLog] [EffectExpiration] deriving (Show, Eq)

data BattleCommandLog =
    BattleCommandLog BattleCommand [ActionResult] deriving (Show, Eq)

