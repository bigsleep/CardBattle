{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

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

data Player = FirstPlayer | SecondPlayer deriving (Show, Eq, Ord, Enum)

data Target =
    TargetAll |
    TargetTeam Player |
    TargetCard Player Int deriving (Show, Eq, Ord)

data BattleErrorType =
    OutOfRange |
    UnknownError
    deriving (Show, Eq, Enum)

data BattleError = BattleError BattleErrorType String deriving (Show, Eq)

instance Error BattleError where
    noMsg = BattleError UnknownError ""
    strMsg s = BattleError UnknownError s

data PropertySet = PropertySet {
    _maxHp :: Int,
    _maxMp :: Int,
    _attack :: Int,
    _defense :: Int,
    _speed :: Int,
    _magic :: Int
    } deriving (Show, Eq)

data Action =
    Defense |
    Attack |
    Heal Int Int |
    Multi [Action]
    deriving (Show, Eq, Ord)

type BattleTurn = ErrorT BattleError (RWS BattleSetting [String] BattleState)

type Targetable = Reader ((BattleSetting, BattleState, Player, Int), Target) (Bool)

data Skill = Skill Action Targetable

data Card = Card {
    _properties :: PropertySet,
    _skills :: [Skill]
    }

data CardState = CardState {
    _hp :: Int,
    _mp :: Int
} deriving (Show)

data BattleEffect = BattleEffect {
    _effectTarget :: Target,
    _effect :: PropertySet -> PropertySet,
    _remaining :: Maybe Int
    }

data BattleSetting = BattleSetting {
    _firstCards :: [Card],
    _secondCards :: [Card],
    _maxTurn :: Maybe Int
    }

data BattleState = BattleState {
    _first :: [CardState],
    _second :: [CardState],
    _effects :: [BattleEffect],
    _remainingTurn :: Maybe Int
    }

data PlayerCommand = PlayerCommand {
    _cardIndex :: Int,
    _skillIndex :: Int,
    _targetIndex :: Int
    } deriving (Show, Eq, Ord)

data BattleCommand = BattleCommand {
    _player :: Player,
    _card :: Int,
    _action :: Action,
    _target :: Target
    } deriving (Show, Eq, Ord)

-- Lenses
$(makeLenses ''PropertySet)
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
