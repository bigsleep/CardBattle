module Battle.BattleSpec where

import qualified Test.Hspec as Hspec

import Battle.Battle
import Battle.Mock
import qualified Battle.Types as T
import qualified Battle.TargetCapacity as TC
import qualified Battle.Skill as Skill

spec :: Hspec.Spec
spec = return ()

properties = T.PropertySet 100 50 40 10 0 0
skills = [Skill.attack]
cards = [T.Card "testCard" properties skills]
setting = T.BattleSetting cards cards (Just 1)
commands = [T.PlayerCommand 0 0 0]
senario = BattleScenario setting [commands] [commands]

spec1 :: Hspec.Spec
spec1 = return ()
