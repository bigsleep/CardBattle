{-# LANGUAGE FlexibleInstances #-}
module Battle.TestUtil where

import qualified Data.List as L (replicate)
import Test.QuickCheck
import Control.Monad(replicateM)
import Control.Applicative
import Control.Lens hiding (Action, elements)

import Battle.Types
import Battle.Battle

defaultCard :: Card
defaultCard = Card {
    _properties = PropertySet {
        _maxHp = 200,
        _maxMp = 100,
        _attack = 50,
        _defense = 20,
        _speed = 10,
        _magic = 10
        },
    _skills = []
    }

defaultTeam :: [Card]
defaultTeam = L.replicate 4 defaultCard

defaultSetting = BattleSetting {
    _firstCards = defaultTeam,
    _secondCards = defaultTeam,
    _maxTurn = Just 2
    }

defaultState = initializeBattleState defaultSetting

-- Arbitrary
instance Arbitrary Player where
    arbitrary = f <$> choose (True, False)
        where f True = FirstPlayer
              f False = SecondPlayer

instance Arbitrary PropertySet where
    arbitrary = do
        hp' <- choose (1, 100)
        mp' <- choose (1, 50)
        attack' <- choose (1, 20)
        defense' <- choose (1, 20)
        speed' <- choose (1, 20)
        magic' <- choose (1, 20)
        return $ PropertySet hp' mp' attack' defense' speed' magic'

instance Arbitrary PropertyFactor where
    arbitrary = do
        let gen = choose (0.1, 10)
        hp' <- gen
        mp' <- gen
        attack' <- gen
        defense' <- gen
        speed' <- gen
        magic' <- gen
        return $ PropertyFactor hp' mp' attack' defense' speed' magic'

instance Arbitrary Card where
    arbitrary = do
        p <- arbitrary
        return $ Card {
            _properties = p,
            _skills = []
            }

instance Arbitrary BattleSetting where
    arbitrary = do
        n <- choose (1, 3)
        m <- choose (1, 3)
        f <- replicateM n arbitrary
        s <- replicateM m arbitrary
        t <- choose (1, 10)
        return $ BattleSetting f s (Just t)

instance Arbitrary Target where
    arbitrary = oneof [genAll, genTeam, genCard]
        where genAll = return TargetAll
              genTeam = arbitrary >>= \p -> return $ TargetTeam p
              genCard = do {p <- arbitrary; c <- choose (0, 10); return $ TargetCard p c}

instance Arbitrary BattleEffect where
    arbitrary = do
        t <- arbitrary
        r <- choose (1, 10)
        factor <- arbitrary
        return $ BattleEffect t factor (Just r)

--
chooseTargetCard :: BattleSetting -> Player -> Gen Int
chooseTargetCard e p = choose (0, cardNum - 1)
    where cardNum = length $ e ^. (playerAccessor p)
