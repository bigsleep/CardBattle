{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

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
import Control.Monad.Reader.Class(ask)
import Control.Monad.Trans.RWS(RWS)
import Control.Monad.Free()
import System.Random()

data PlayerTag = FirstPlayer | SecondPlayer deriving (Show, Eq, Ord, Enum)

data CardPosition = Card1 | Card2 | Card3 deriving (Show, Eq, Ord, Enum)

data Target = forall a. (TargetT a, Show a) => Target a
data TargetAll = TargetAll deriving (Show, Eq)
data TargetTeam = TargetTeam PlayerTag deriving (Show, Eq)
data TargetCard = TargetCard PlayerTag CardPosition deriving (Show, Eq, Ord)

class TargetT a where
    onTarget :: PlayerTag -> CardPosition -> a -> Bool

instance TargetT TargetAll where
    onTarget _ _ TargetAll = True

instance TargetT TargetTeam where
    onTarget q _ (TargetTeam p) = p == q

instance TargetT TargetCard where
    onTarget q y (TargetCard p x) = p == q && x == y

instance TargetT Target where
    onTarget q y (Target x) = onTarget q y x

instance Show Target where
    show (Target x) = "Target " ++ show x

data PropertySet = PropertySet {
    _maxHp :: Int,
    _maxMp :: Int,
    _attack :: Int,
    _defense :: Int,
    _speed :: Int,
    _magic :: Int
    } deriving (Show)

data MagicT =
    BoostAttack TargetCard |
    BoostDefense TargetCard |
    BoostSpeed TargetCard |
    BoostMagic TargetCard |
    Heal TargetCard
    deriving (Show, Eq, Ord)

data Action =
    Defense |
    Attack TargetCard |
    Magic MagicT
    deriving (Show, Eq, Ord)

data Card = Card {
    _properties :: PropertySet,
    _skills :: Set Action
    } deriving (Show)

data CardState = CardState {
    _hp :: Int,
    _mp :: Int
} deriving (Show)

data BattleEffect = BattleEffect {
    _target :: Target,
    _effect :: PropertySet -> PropertySet,
    _remaining :: Maybe Int
    }

data BattleSetting = BattleSetting {
    _firstCards :: Map CardPosition Card,
    _secondCards :: Map CardPosition Card,
    _maxTurn :: Maybe Int
    } deriving (Show)

data BattleState = BattleState {
    _first :: Map CardPosition CardState,
    _second :: Map CardPosition CardState,
    _effects :: [BattleEffect],
    _remainingTurn :: Maybe Int
    }

data PlayerCommand = PlayerCommand {
    _card :: CardPosition,
    _action :: Action
    } deriving (Show, Eq, Ord)

data BattleCommand = BattleCommand {
    _player :: PlayerTag,
    _command :: PlayerCommand
    } deriving (Show, Eq, Ord)


-- Lenses
$(makeLenses ''PropertySet)
$(makeLenses ''MagicT)
$(makeLenses ''Action)
$(makeLenses ''Card)
$(makeLenses ''CardState)
$(makeLenses ''BattleSetting)
$(makeLenses ''BattleState)
$(makeLenses ''PlayerCommand)
$(makeLenses ''BattleEffect)
$(makeLenses ''BattleCommand)

playerAccessor :: PlayerTag -> Lens' BattleSetting (Map CardPosition Card)
playerAccessor FirstPlayer = firstCards
playerAccessor SecondPlayer = secondCards

playerStateAccessor :: PlayerTag -> Lens' BattleState (Map CardPosition CardState)
playerStateAccessor FirstPlayer = first
playerStateAccessor SecondPlayer = second
