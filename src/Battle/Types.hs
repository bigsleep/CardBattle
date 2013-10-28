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
import Control.Monad.Error(ErrorT, Error, noMsg, strMsg, throwError)

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
$(makeFields ''PropertySet)

data PropertyFactor = PropertyFactor {
    _propertyfactorMaxHp :: Double,
    _propertyfactorMaxMp :: Double,
    _propertyfactorAttack :: Double,
    _propertyfactorDefense :: Double,
    _propertyfactorSpeed :: Double,
    _propertyfactorMagic :: Double
    } deriving (Show, Eq)
$(makeFields ''PropertyFactor)
    
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
    TargetCapacityAlive |
    TargetCapacityDead |
    TargetCapacityMixAnd [TargetCapacity] |
    TargetCapacityMixOr [TargetCapacity]
    deriving (Show, Eq)

data Skill = Skill {
    _skillAction :: Action,
    _skillTarget :: TargetCapacity
    } deriving (Show, Eq)
$(makeFields ''Skill)

data Card = Card {
    _cardName :: String,
    _cardProperties :: PropertySet,
    _cardSkills :: [Skill]
    } deriving (Show, Eq)
$(makeFields ''Card)

data CardState = CardState {
    _cardstateHp :: Int,
    _cardstateMp :: Int
} deriving (Show, Eq)
$(makeFields ''CardState)

data BattleEffect = BattleEffect {
    _battleeffectAction :: Action,
    _battleeffectTarget :: Target,
    _battleeffectFactor :: PropertyFactor,
    _battleeffectRemaining :: Maybe Int
    } deriving (Show, Eq)
$(makeFields ''BattleEffect)

data BattleSetting = BattleSetting {
    _battlesettingFirst :: [Card],
    _battlesettingSecond :: [Card],
    _battlesettingMaxTurn :: Maybe Int
    } deriving (Show, Eq)
$(makeFields ''BattleSetting)

data BattleState = BattleState {
    _battlestateFirst :: [CardState],
    _battlestateSecond :: [CardState],
    _battlestateOneTurnEffects :: [BattleEffect],
    _battlestateEffects :: [BattleEffect],
    _battlestateTurn :: Int
    } deriving (Show, Eq)
$(makeFields ''BattleState)

data PlayerCommand = PlayerCommand {
    _playercommandCardIndex :: Int,
    _playercommandSkillIndex :: Int,
    _playercommandTargetIndex :: Int
    } deriving (Show, Eq)
$(makeFields ''PlayerCommand)

data BattleCommand = BattleCommand {
    _battlecommandPlayer :: Player,
    _battlecommandCard :: Int,
    _battlecommandAction :: Action,
    _battlecommandTarget :: Target
    } deriving (Show, Eq)
$(makeFields ''BattleCommand)

data ActionChoice = ActionChoice {
    _actionchoiceSkillIndex :: Int,
    _actionchoiceAction :: Action,
    _actionchoiceTargets :: [Target]
    } deriving (Show, Eq)
$(makeFields ''ActionChoice)

data CommandChoice = CommandChoice {
    _commandchoiceCardIndex :: Int,
    _commandchoiceActions :: [ActionChoice]
    } deriving (Show, Eq)
$(makeFields ''CommandChoice)

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

onTarget :: Player -> Int -> Target -> Bool
onTarget _ _ TargetAll = True
onTarget q _ (TargetTeam p) = p == q
onTarget q y (TargetCard p x) = p == q && x == y

defaultProperties :: Player -> Int -> BattleTurn PropertySet
defaultProperties p c = do
    e <- ask
    case card' e of
         Nothing -> throwError "in defaultProperties. list index out of range."
         Just c  -> return (c ^. properties)
    where card' x = cards x ^? ix c
          cards x = (x ^. (playerAccessor p))

unitPropertyFactor :: PropertyFactor
unitPropertyFactor = PropertyFactor 1 1 1 1 1 1

applyPropertyFactor :: PropertySet -> PropertyFactor -> PropertySet
applyPropertyFactor (PropertySet a1 b1 c1 d1 e1 f1) (PropertyFactor a2 b2 c2 d2 e2 f2) =
    PropertySet (floor $ fromIntegral a1 * a2)
                (floor $ fromIntegral b1 * b2)
                (floor $ fromIntegral c1 * c2)
                (floor $ fromIntegral d1 * d2)
                (floor $ fromIntegral e1 * e2)
                (floor $ fromIntegral f1 * f2)

multPropertyFactor :: PropertyFactor -> PropertyFactor -> PropertyFactor
multPropertyFactor (PropertyFactor a1 b1 c1 d1 e1 f1) (PropertyFactor a2 b2 c2 d2 e2 f2) =
    PropertyFactor (a1 * a2) (b1 * b2) (c1 * c2) (d1 * d2) (e1 * e2) (f1 * f2)

subPropertySet :: PropertySet -> PropertySet -> PropertySet
subPropertySet (PropertySet a b c d e f) (PropertySet a' b' c' d' e' f') = PropertySet (a - a') (b - b') (c - c') (d - d') (e - e') (f - f')

currentProperties :: Player -> Int -> BattleTurn PropertySet
currentProperties p c = do
    e <- ask
    s <- get
    case (currentProperties' e s p c) of
         Nothing -> throwError "in currentProperties. list index out of range."
         Just x  -> return x

currentProperties' :: BattleSetting -> BattleState -> Player -> Int -> Maybe PropertySet
currentProperties' e s p c = fmap applyEffect card'
    where card' = (e ^. (playerAccessor p)) ^? ix c
          applyEffect (Card _ q _) = applyPropertyFactor q eff
          eff = foldl multPropertyFactor unitPropertyFactor (effectFactors (s ^. oneTurnEffects ++ s ^. effects))
          effectFactors x = map (^. factor) $ filter ((onTarget p c) . (^. target)) x
