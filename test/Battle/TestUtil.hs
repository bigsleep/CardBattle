module Battle.TestUtil where

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
defaultTeam = [defaultCard]

defaultSetting = BattleSetting {
    _firstCards = defaultTeam,
    _secondCards = defaultTeam,
    _maxTurn = Just 2
    }

defaultState = initializeBattleState defaultSetting
