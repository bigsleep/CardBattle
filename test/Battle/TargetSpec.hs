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


    prop "ターゲット全列挙" $ do
        e <- (arbitrary :: Gen BattleSetting)
        let cardNum p = length (e ^. playerAccessor p)
        let tcards p = map (\c -> TargetCard p c) [0..(cardNum p - 1)]
        let first' = tcards FirstPlayer
        let second' = tcards SecondPlayer
        let rest = [TargetTeam FirstPlayer, TargetTeam SecondPlayer, TargetAll]
        let ans = first' ++ second' ++ rest
        return $ enumerateAllTargets e == ans


    prop "ターゲット列挙、すべて可能の場合" $ do
        let p = FirstPlayer
        let c = 0
        e <- (arbitrary :: Gen BattleSetting)
        let s = initializeBattleState e
        let test = enumerateTargets e s p c targetableAlmighty
        let ans = enumerateAllTargets e
        return $ test == ans


    prop "ターゲット列挙、ターゲット可能自分のみの場合" $ do
        p <- arbitrary
        c <- arbitrary
        e <- arbitrary
        let s = initializeBattleState e
        let l = length $ e ^. (playerAccessor p)
        let test = enumerateTargets e s p c targetableSelf
        let ans = if 0 <= c && c < l then [TargetCard p c] else []
        let m = "case TeamSize: " ++ show l ++ " Player: " ++ show p ++ " CardIndex: " ++ show c
        if test == ans
            then return Prop.succeeded
            else return Prop.failed {Prop.reason = m}


    prop "ターゲット列挙、敵チームと自分" $ do
        player' <- arbitrary
        setting' <- arbitrary
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

