module Battle.TargetSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Control.Lens hiding (Action)
import Battle.Types
import Battle.Target
import Battle.TestUtil

spec :: Spec
spec = do
    describe "Target" $ do
    it "enumerateAllTragets" $ do
        let cardNum p s = length (s ^. playerAccessor p)
        let tcards p s = map (\c -> TargetCard p c) [0..(cardNum p s - 1)]
        let first' = tcards FirstPlayer defaultSetting
        let second' = tcards SecondPlayer defaultSetting
        let rest = [TargetTeam FirstPlayer, TargetTeam SecondPlayer, TargetAll]
        let ans = first' ++ second' ++ rest
        enumerateAllTargets defaultSetting `shouldBe` ans


    it "enumerateTargets, targetableAlmighty" $ do
        let p = FirstPlayer
        let c = 0
        let test = enumerateTargets defaultSetting defaultState p c targetableAlmighty
        let ans = enumerateAllTargets defaultSetting
        test `shouldBe` ans


    prop "ターゲット可能自分のみの場合, ターゲット列挙" $ \p c -> do
        let s = defaultSetting
        let l = length $ s ^. (playerAccessor p)
        let test = enumerateTargets defaultSetting defaultState p c targetableSelf
        let ans = if 0 <= c && c < l then [TargetCard p c] else []
        let m = "case TeamSize: " ++ show l ++ " Player: " ++ show p ++ " CardIndex: " ++ show c
        if test == ans
            then Prop.succeeded
            else Prop.failed {Prop.reason = m}
