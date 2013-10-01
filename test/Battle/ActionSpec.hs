module Battle.ActionSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Control.Lens hiding (Action)

import Battle.Types
import Battle.Target
import Battle.Battle
import Battle.Action
import Battle.TestUtil

spec :: Spec
spec = do
    describe "Action" $ do

    prop "currentPropertiesエフェクトなし" $ do
        e <- arbitrary
        p <- arbitrary
        let cardNum = length $ e ^. playerAccessor p
        c <- choose (0, cardNum - 1)
        let test = currentProperties e [] p c
        return $ test == ((e ^. (playerAccessor p)) !! c ^. properties)


    prop "currentProperties、攻撃力2倍2倍" $ do
        e <- arbitrary
        p <- arbitrary
        let cardNum = length $ e ^. playerAccessor p
        c <- choose (0, cardNum - 1)
        let eff = \a -> a{_attack = 2 * (_attack a)}
        let doubleAttack = BattleEffect TargetAll eff (Just 1)
        let test = currentProperties e [doubleAttack, doubleAttack] p c
        let ans = eff . eff $ ((e ^. (playerAccessor p)) !! c ^. properties)
        return $ test == ans


    prop "currentProperties、エフェクト対象外" $ do
        e <- arbitrary
        p <- arbitrary
        let opponent = if p == FirstPlayer then SecondPlayer else FirstPlayer
        let cardNum = length $ e ^. playerAccessor p
        c <- choose (0, cardNum - 1)
        effect' <- arbitrary
        let target' = TargetTeam opponent
        let test = currentProperties e [(effect'{_effectTarget = target'})] p c
        let ans = ((e ^. (playerAccessor p)) !! c ^. properties)
        return $ test == ans


    prop "currentProperties、エフェクトランダム" $ do
        setting' <- arbitrary
        effects' <- arbitrary
        p <- arbitrary
        let cardNum = length $ setting' ^. playerAccessor p
        c <- choose (0, cardNum - 1)
        let test = currentProperties setting' effects' p c
        let eff = filter ((onTarget p c) . (^. effectTarget)) effects'
        let f = foldl (.) id (map (^. effect) eff)
        let ans = f ((setting' ^. (playerAccessor p)) !! c ^. properties)
        return $ test == ans

