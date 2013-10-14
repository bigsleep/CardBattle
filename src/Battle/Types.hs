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
    _battlesettingFirstCards :: [Card],
    _battlesettingSecondCards :: [Card],
    _battlesettingMaxTurn :: Maybe Int
    } deriving (Show, Eq)

data BattleState = BattleState {
    _battlestateFirst :: [CardState],
    _battlestateSecond :: [CardState],
    _battlestateOneTurnEffects :: [BattleEffect],
    _battlestateEffects :: [BattleEffect],
    _battlestateRemainingTurn :: Maybe Int
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

playerAccessor :: Player -> Lens' BattleSetting [Card]
playerAccessor FirstPlayer = firstCards
playerAccessor SecondPlayer = secondCards

playerStateAccessor :: Player -> Lens' BattleState [CardState]
playerStateAccessor FirstPlayer = first
playerStateAccessor SecondPlayer = second

propertyFactorAccessor :: PropertyFactorTag -> Lens' PropertyFactor Double
propertyFactorAccessor MaxHpFactor = maxHp
propertyFactorAccessor MaxMpFactor = maxMp
propertyFactorAccessor AttackFactor = attack
propertyFactorAccessor DefenseFactor = defense
propertyFactorAccessor SpeedFactor = speed
propertyFactorAccessor MagicFactor = magic

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

data BattleLog = BattleLog BattleState [BattleCommandLog] [EffectExpiration] deriving (Show, Eq)

data BattleCommandLog =
    BattleCommandLog BattleCommand [ActionResult] deriving (Show, Eq)

