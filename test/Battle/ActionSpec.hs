module Battle.ActionSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Control.Monad.RWS
import Control.Monad.Error
import Control.Lens hiding (Action)

import Battle.Types
import Battle.Target
import Battle.Battle
import Battle.Action
import Battle.TestUtil

spec :: Spec
spec = do
    describe "Action" $ do

    prop "currentProperties エフェクトなし" $ do
        e <- arbitrary
        let s = initializeBattleState e
        p <- arbitrary
        c <- chooseTargetCard e p
        let test = runCurrentPropertiesTest e s p c
        let expected = ((e ^. (playerAccessor p)) !! c ^. properties)
        case test of
             Left s -> return False
             Right result -> return $ result == expected


    prop "currentProperties 攻撃力2倍2倍" $ do
        setting' <- arbitrary
        p <- arbitrary
        c <- chooseTargetCard setting' p
        let eff = \a -> a & attack %~ (*2)
        let doubleAttack = BattleEffect TargetAll eff (Just 1)
        let effects' = [doubleAttack, doubleAttack]
        let state' = (initializeBattleState setting') & effects .~ effects'
        let test = runCurrentPropertiesTest setting' state' p c
        let expected = eff . eff $ ((setting' ^. (playerAccessor p)) !! c ^. properties)
        case test of
             Left s -> return False
             Right result -> return $ result == expected


    prop "currentProperties エフェクトランダム" $ do
        setting' <- arbitrary
        effects' <- arbitrary
        let state' = (initializeBattleState setting') & effects .~ effects'
        p <- arbitrary
        c <- chooseTargetCard setting' p
        let test = runCurrentPropertiesTest setting' state' p c
        let eff = filter ((onTarget p c) . (^. effectTarget)) effects'
        let f = foldl (.) id (map (^. effect) eff)
        let expected = f ((setting' ^. (playerAccessor p)) !! c ^. properties)
        case test of
             Left s -> return False
             Right result -> return $ result == expected


    prop "currentProperties エフェクト対象外" $ do
        setting' <- arbitrary
        p <- arbitrary
        c <- chooseTargetCard setting' p
        tc <- chooseTargetCard setting' (opponentPlayer p)
        effect' <- arbitrary
        let target' = TargetCard (opponentPlayer p) tc
        let state' = (initializeBattleState setting') & effects .~ [(effect' & effectTarget .~ target')]
        let test = runCurrentPropertiesTest setting' state' p c
        let expected = ((setting' ^. (playerAccessor p)) !! c ^. properties)
        case test of
             Left s -> return False
             Right result -> return $ result == expected


runCurrentPropertiesTest :: BattleSetting -> BattleState -> Player -> Int -> Either String PropertySet
runCurrentPropertiesTest e s p c = result
    where m = currentProperties p c
          (result, _, _) = runRWS (runErrorT m) e s
