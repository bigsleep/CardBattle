module Battle.BattleSpec where

import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Control.Monad.Reader
import Control.Lens

import Battle.Mock
import Battle.TestUtil
import qualified Battle.Types as T
import qualified Battle.TargetCapacity as TC
import qualified Battle.Skill as S
import qualified Battle.Battle as B

spec :: Hspec.Spec
spec = battleAttackAttackSpec


battleAttackAttackSpec :: Hspec.Spec
battleAttackAttackSpec = prop "1ターン攻撃攻撃" $
    \properties ->
        let hp = properties ^. T.maxHp
            mp = properties ^. T.maxMp
            attack = properties ^. T.attack
            defense = properties ^. T.defense
            skills = [S.attack]
            cards = [T.Card "testCard" properties skills]
            setting = T.BattleSetting cards cards (Just 1)
            commands = [T.PlayerCommand 0 0 0]
            senario = BattleScenario setting [commands] [commands]
            damage = max 1 (attack - defense)
            afterHp = max 0 (hp - damage)
            expectedCardState = T.CardState afterHp mp
            expectedState = T.BattleState [expectedCardState] [expectedCardState] [] [] 1
            m = runBattleOnMockIO
            Right (_, (_, s), _) = runReader m senario
            message = "result: " ++ show s ++ "\nexpected: " ++ show expectedState
            result = if s == expectedState
                then P.succeeded
                else P.failed {P.reason = message}
        in result

