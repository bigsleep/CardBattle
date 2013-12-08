module Battle.BattleSpec where

import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Control.Monad.Reader

import Battle.Mock
import qualified Battle.Types as T
import qualified Battle.TargetCapacity as TC
import qualified Battle.Skill as S
import qualified Battle.Battle as B

spec :: Hspec.Spec
spec = spec1


spec1 :: Hspec.Spec
spec1 = prop "1ターン攻撃のみ" result
    where hp = 100
          mp = 0
          attack = 50
          defense = 10
          properties = T.PropertySet hp mp attack defense 0 0
          skills = [S.attack]
          cards = [T.Card "testCard" properties skills]
          setting = T.BattleSetting cards cards (Just 1)
          commands = [T.PlayerCommand 0 0 0]
          senario = BattleScenario setting [commands] [commands]
          expectedCardState = T.CardState (hp - (attack - defense)) mp
          expectedState = T.BattleState [expectedCardState] [expectedCardState] [] [] 1
          m = runBattleOnMockIO
          Right (_, (_, s), _) = runReader m senario
          message = show s
          result = if s == expectedState
            then P.succeeded
            else P.failed {P.reason = message}
