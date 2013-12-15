module Battle.BattleSpec where

import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Positive(..))
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
     >> battleAttackHealSpec


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



battleAttackHealSpec :: Hspec.Spec
battleAttackHealSpec = prop "1ターン攻撃回復" $
    \properties (Positive useMp) ->
        let hp = properties ^. T.maxHp
            mp = properties ^. T.maxMp
            attack = properties ^. T.attack
            defense = properties ^. T.defense
            magic = properties ^. T.magic
            healFactor = P.factorDenominator
            attackSkills = [T.Skill (T.Attack P.factorDenominator) TC.aliveOpponentOne]
            healSkills = [T.Skill (T.Heal healFactor useMp) TC.self]
            attackCards = [T.Card "testCard" properties attackSkills]
            healCards = [T.Card "testCard" properties healSkills]
            setting = T.BattleSetting attackCards healCards (Just 1)
            commands = [T.PlayerCommand 0 0 0]
            healCommands = if mp >= useMp
                              then commands
                              else []
            senario = BattleScenario setting [commands] [healCommands]
            damage = max 1 (attack - defense)
            healHp = magic * healFactor `div` P.factorDenominator
            expectAfterHp alive canHeal = case (alive, canHeal) of
                                               (False, _) -> 0
                                               (True, False) -> hp - damage
                                               (True, True) -> min hp (hp - damage + healHp)
            afterHp = expectAfterHp (damage < hp) (mp >= useMp)
            afterMp = if damage < hp && mp >= useMp
                         then mp - useMp
                         else mp
            expectedCardState = T.CardState afterHp afterMp
            expectedState = T.BattleState [T.CardState hp mp] [expectedCardState] [] [] 1
            m = runBattleOnMockIO
            (s, w) = case runReader m senario of
                          Right (_, (_, a), b) -> (a, b)
                          Left a -> error a
            message = "result: " ++ show s ++ "\nexpected: " ++ show expectedState ++ "\nlog: " ++ show w
            result = if s == expectedState
                then P.succeeded
                else P.failed {P.reason = message}
        in result
