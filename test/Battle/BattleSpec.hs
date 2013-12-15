module Battle.BattleSpec where

import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ()
import qualified Test.QuickCheck.Property as P
import Control.Monad.Reader
import Control.Lens ((^.))

import Battle.Mock
import Battle.TestUtil ()
import qualified Battle.Types as T
import qualified Battle.Property as P
import qualified Battle.TargetCapacity as TC

spec :: Hspec.Spec
spec =  battleAttackAttackSpec
     >> battleAttackDefenseSpec


battleAttackAttackSpec :: Hspec.Spec
battleAttackAttackSpec = prop "1ターン攻撃攻撃" $
    \properties ->
        let hp = properties ^. T.maxHp
            mp = properties ^. T.maxMp
            attack = properties ^. T.attack
            defense = properties ^. T.defense
            skills =[T.Skill (T.Attack P.factorDenominator) TC.aliveOpponentOne]
            cards = [T.Card "testCard" properties skills]
            setting = T.BattleSetting cards cards (Just 1)
            commands = [T.PlayerCommand 0 0 0]
            senario = BattleScenario setting [commands] [commands]
            damage = max 1 (attack - defense)
            afterHp = max 0 (hp - damage)
            expectedCardState = T.CardState afterHp mp
            expectedState = if afterHp > 0
                               then T.BattleState [expectedCardState] [expectedCardState] [] [] 1
                               else T.BattleState [T.CardState hp mp] [expectedCardState] [] [] 1
            m = runBattleOnMockIO
            Right (_, (_, s), _) = runReader m senario
            message = "result: " ++ show s ++ "\nexpected: " ++ show expectedState
            result = if s == expectedState
                then P.succeeded
                else P.failed {P.reason = message}
        in result


battleAttackDefenseSpec :: Hspec.Spec
battleAttackDefenseSpec = prop "1ターン攻撃防御" $
    \properties ->
        let hp = properties ^. T.maxHp
            mp = properties ^. T.maxMp
            attack = properties ^. T.attack
            defense = properties ^. T.defense
            defenseFactor = P.factorDenominator * 2
            attackSkills = [T.Skill (T.Attack P.factorDenominator) TC.aliveOpponentOne]
            defenseSkills = [T.Skill (T.Defense defenseFactor) TC.self] 
            attackCards = [T.Card "testCard" properties attackSkills]
            defenseCards = [T.Card "testCard" properties defenseSkills]
            setting = T.BattleSetting attackCards defenseCards (Just 1)
            commands = [T.PlayerCommand 0 0 0]
            senario = BattleScenario setting [commands] [commands]
            damage = max 1 (attack - defense * defenseFactor `div` P.factorDenominator)
            afterHp = max 0 (hp - damage)
            expectedCardState = T.CardState afterHp mp
            expectedState = T.BattleState [T.CardState hp mp] [expectedCardState] [] [] 1
            m = runBattleOnMockIO
            Right (_, (_, s), w) = runReader m senario
            message = "result: " ++ show s ++ "\nexpected: " ++ show expectedState ++ "\nlog: " ++ show w
            result = if s == expectedState
                then P.succeeded
                else P.failed {P.reason = message}
        in result
