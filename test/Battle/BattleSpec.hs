module Battle.BattleSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Text.Printf

import Battle.Battle
import Battle.Mock

spec :: Spec
spec = return ()

{-
prop1 = PropertySet 100 50 40 10 0 0
skills1 = [Skill Attack (TargetCapacityMixAnd [TargetCapacityOne, TargetCapacityOpponent, TargetCapacityAlive])]
card1 = Card "testCard" testProperty testSkills
setting1 = BattleSetting testCard testCard (Just 1)
commands1 = [PlayerCommand 0 0 0]
senario1 = BattleScenario testBattleSetting commands commands

spec1 :: Spec
spec1 =
    prop "1ターン両方攻撃"
-}
