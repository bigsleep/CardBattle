module Battle.TargetSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Control.Applicative(liftA2)
import Control.Lens hiding (Action)
import Battle.Types
import Battle.Target
import Battle.Battle
import Battle.TestUtil
import Text.Printf

spec :: Spec
spec = do
    describe "Target" $ do

    prop "ターゲット全列挙" $ \e ->
        let cardNum p = length (e ^. playerAccessor p)
            tcards p = map (\c -> TargetCard p c) [0..(cardNum p - 1)]
            first' = tcards FirstPlayer
            second' = tcards SecondPlayer
            rest = [TargetTeam FirstPlayer, TargetTeam SecondPlayer, TargetAll]
            ans = first' ++ second' ++ rest
        in  enumerateAllTargets e == ans


    prop "ターゲット列挙、すべて可能の場合" $ \e->
        let p = FirstPlayer
            c = 0
            s = initializeBattleState e
            test = enumerateTargets e s p c targetableAlmighty
            ans = enumerateAllTargets e
        in  test == ans


    prop "ターゲット列挙、ターゲット可能自分のみの場合" $ \e p c ->
        let s = initializeBattleState e
            l = length $ e ^. (playerAccessor p)
            test = enumerateTargets e s p c targetableSelf
            ans = if 0 <= c && c < l then [TargetCard p c] else []
            m = "case TeamSize: " ++ show l ++ " Player: " ++ show p ++ " CardIndex: " ++ show c
        in if test == ans
              then Prop.succeeded
              else Prop.failed {Prop.reason = m}


    prop "ターゲット列挙、敵チームと自分" $ \player' setting' -> do
        let state' = initializeBattleState setting'
        let cardNum = length $ setting' ^. (playerAccessor player')
        card' <- choose (0, cardNum - 1)
        let and' = liftA2 (&&)
        let or' = liftA2 (||)
        let targetable = targetableSelf `or'` (targetableTeam `and'` targetableOpponent)
        let test = enumerateTargets setting' state' player' card' targetable
        let ans = [TargetCard player' card', TargetTeam $ if player' == FirstPlayer then SecondPlayer else FirstPlayer]
        let message = printf "player: %s\ncard: %d\ncardNum: %d\nexpected: %s\nresult: %s\n"
                      (show player') card' cardNum (show ans) (show test)
        if test == ans
            then return Prop.succeeded
            else return Prop.failed {Prop.reason = message}

