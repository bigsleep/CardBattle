module Battle.TargetSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (choose)
import qualified Test.QuickCheck.Property as Prop (Result(..), failed, succeeded)
import Control.Applicative(liftA2)
import Control.Lens hiding (Action)

import qualified Battle.Types as T
import qualified Battle.Target as Target
import qualified Battle.Battle as B
import Battle.TestUtil ()
import Text.Printf

spec :: Spec
spec = do
    describe "Target" $ do

    prop "ターゲット全列挙" $ \e ->
        let cardNum p = length (e ^. T.playerAccessor p)
            tcards p = map (\c -> T.TargetCard p c) [0..(cardNum p - 1)]
            first' = tcards T.FirstPlayer
            second' = tcards T.SecondPlayer
            rest = [T.TargetTeam T.FirstPlayer, T.TargetTeam T.SecondPlayer, T.TargetAll]
            ans = first' ++ second' ++ rest
        in  Target.enumerateAllTargets e == ans


    prop "ターゲット列挙、すべて可能の場合" $ \e->
        let p = T.FirstPlayer
            c = 0
            s = B.initializeBattleState e
            test = Target.enumerateTargets e s p c (Target.targetable T.TcAlmighty)
            ans = Target.enumerateAllTargets e
        in  test == ans


    prop "ターゲット列挙、ターゲット可能自分のみの場合" $ \e p c ->
        let s = B.initializeBattleState e
            l = length $ e ^. (T.playerAccessor p)
            test = Target.enumerateTargets e s p c (Target.targetable T.TcSelf)
            ans = if 0 <= c && c < l then [T.TargetCard p c] else []
            m = "case TeamSize: " ++ show l ++ " Player: " ++ show p ++ " CardIndex: " ++ show c
        in if test == ans
              then Prop.succeeded
              else Prop.failed {Prop.reason = m}


    prop "ターゲット列挙、敵チームと自分" $ \player' setting' -> do
        let state' = B.initializeBattleState setting'
        let cardNum = length $ setting' ^. (T.playerAccessor player')
        card' <- choose (0, cardNum - 1)
        let or' = liftA2 (||)
        let targetable' = Target.targetable T.TcSelf `or'`  Target.targetable T.TcOpponentTeam
        let test = Target.enumerateTargets setting' state' player' card' targetable'
        let ans = [T.TargetCard player' card', T.TargetTeam $ if player' == T.FirstPlayer then T.SecondPlayer else T.FirstPlayer]
        let message = printf "player: %s\ncard: %d\ncardNum: %d\nexpected: %s\nresult: %s\n"
                      (show player') card' cardNum (show ans) (show test)
        if test == ans
            then return Prop.succeeded
            else return Prop.failed {Prop.reason = message}


    prop "ターゲット列挙、相手チームで生きてるカード1体" $ \player' setting' -> do
        let state' = B.initializeBattleState setting' & T.playerAccessor (T.opponentPlayer player') %~ map (& T.hp .~ 0) :: T.BattleState
        let cardNum = length $ setting' ^. (T.playerAccessor player')
        card' <- choose (0, cardNum - 1)
        let targetable' = Target.targetable T.TcAliveOpponentOne
        let test = Target.enumerateTargets setting' state' player' card' targetable'
        let message = printf "player: %s\ncard: %d\ncardNum: %d\nexpected: []\nresult: %s\nstate: %s\n"
                      (show player') card' cardNum (show test) (show state')
        if test == []
            then return Prop.succeeded
            else return Prop.failed {Prop.reason = message}

