module Battle.TestUtil where

import qualified Data.List as L (replicate)
import Test.QuickCheck
import Control.Applicative
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
    

