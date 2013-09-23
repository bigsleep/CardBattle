module Battle.TestUtil where

import Battle.Types

defaultCard :: Card
defaultCard = Card {
    _properties = PropertySet {
        _maxHp = 200,
        _maxMp = 100,
        _attack = 50,
        _defense = 20,
        _speed = 10,
        _magic = 10
        }
    }

defaultTeam :: Map CardPosition Card
defaultTeam = fromList [(Card1, defaultCard), (Card2, defaultCard), (Card3, defaultCard)]

defaultSetting = BattleSetting {
    _firstCards = defaultTeam,
    _secondCards = defaultTeam,
    _maxTurn = Just 2
    }

defaultState = initializeBattleState defaultSetting
