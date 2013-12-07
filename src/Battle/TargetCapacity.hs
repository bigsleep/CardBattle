module Battle.TargetCapacity where

import Battle.Types

aliveOne :: TargetCapacity
aliveOne = TargetCapacityMixAnd [TargetCapacityOne, TargetCapacityAlive]

aliveTeam :: TargetCapacity
aliveTeam = TargetCapacityMixAnd [TargetCapacityTeam, TargetCapacityAlive]

aliveAll :: TargetCapacity
aliveAll = TargetCapacityMixAnd [TargetCapacityAll, TargetCapacityAlive]

aliveOpponentOne :: TargetCapacity
aliveOpponentOne = TargetCapacityMixAnd [aliveOne, TargetCapacityOpponent]

aliveOpponentTeam :: TargetCapacity
aliveOpponentTeam = TargetCapacityMixAnd [aliveTeam, TargetCapacityOpponent]

aliveOwnOne :: TargetCapacity
aliveOwnOne = TargetCapacityMixAnd [aliveOne, TargetCapacityOwn]

aliveOwnTeam :: TargetCapacity
aliveOwnTeam = TargetCapacityMixAnd [aliveTeam, TargetCapacityOwn]

deadOne :: TargetCapacity
deadOne = TargetCapacityMixAnd [TargetCapacityOne, TargetCapacityDead]

deadTeam :: TargetCapacity
deadTeam = TargetCapacityMixAnd [TargetCapacityTeam, TargetCapacityDead]

deadAll :: TargetCapacity
deadAll = TargetCapacityMixAnd [TargetCapacityAll, TargetCapacityDead]

deadOpponentOne :: TargetCapacity
deadOpponentOne = TargetCapacityMixAnd [deadOne, TargetCapacityOpponent]

deadOpponentTeam :: TargetCapacity
deadOpponentTeam = TargetCapacityMixAnd [deadTeam, TargetCapacityOpponent]

deadOwnOne :: TargetCapacity
deadOwnOne = TargetCapacityMixAnd [deadOne, TargetCapacityOwn]

deadOwnTeam :: TargetCapacity
deadOwnTeam = TargetCapacityMixAnd [deadTeam, TargetCapacityOwn]

self :: TargetCapacity
self = TargetCapacitySelf
