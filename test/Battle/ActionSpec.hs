module Battle.ActionSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Control.Monad.RWS
import Control.Monad.Error
import Control.Lens hiding (Action)
import Debug.Trace
import Text.Printf

import Battle.Types
import Battle.Target
import Battle.Battle
import Battle.Action
import Battle.TestUtil

spec :: Spec
spec = do
    describe "Action" $ do

    prop "currentProperties エフェクトなし" $ \e p -> do
        c <- chooseTargetCard e p
        let s = initializeBattleState e
        let test = runCurrentPropertiesTest e s p c
        let expected = ((e ^. (playerAccessor p)) !! c ^. properties)
        case test of
             Left s -> return False
             Right result -> return $ result == expected


    prop "currentProperties 攻撃力2倍2倍" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let factor = unitPropertyFactor {_attackFactor = 2}
        let doubleAttack = BattleEffect (Boost AttackFactor 2) TargetAll factor (Just 1)
        let effects' = [doubleAttack, doubleAttack]
        let state' = (initializeBattleState setting') & effects .~ effects'
        let test = runCurrentPropertiesTest setting' state' p c
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = before `applyPropertyFactor` (factor `multPropertyFactor` factor)
        case test of
             Left s -> return False
             Right result -> return $ result == expected


    prop "currentProperties エフェクトランダム" $ \setting' effects' p -> do
        c <- chooseTargetCard setting' p
        let state' = (initializeBattleState setting') & effects .~ effects'
        let test = runCurrentPropertiesTest setting' state' p c
        let es = map (^. factor) $ filter ((onTarget p c) . (^. effectTarget)) effects'
        let f = foldl multPropertyFactor unitPropertyFactor es
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = before `applyPropertyFactor` f
        case test of
             Left s -> return False
             Right result -> return $ result == expected


    prop "currentProperties エフェクト対象外" $ \setting' effect' p -> do
        c <- chooseTargetCard setting' p
        tc <- chooseTargetCard setting' (opponentPlayer p)
        let target' = TargetCard (opponentPlayer p) tc
        let state' = (initializeBattleState setting') & effects .~ [(effect' & effectTarget .~ target')]
        let test = runCurrentPropertiesTest setting' state' p c
        let expected = ((setting' ^. (playerAccessor p)) !! c ^. properties)
        case test of
             Left s -> return False
             Right result -> return $ result == expected

    
    prop "execAction Attack 単体攻撃" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let t = opponentPlayer p
        tc <- chooseTargetCard setting' t
        let target' = TargetCard t tc
        let state' = initializeBattleState setting'
        let properties' = ((setting' ^. (playerAccessor p)) !! c ^. properties)
        let tproperties' = ((setting' ^. (playerAccessor t)) !! tc ^. properties)
        let attack' = properties' ^. attack
        let defense' = tproperties' ^. defense
        let hp' = tproperties' ^. maxHp
        let damage = min hp' (max 1 (attack' - defense'))
        let test = runExecActionTest setting' state' p c Attack target'
        let expectedState = state' & playerStateAccessor t . ix tc . hp .~ (hp' - damage)
        let expected = expectedState ^. playerStateAccessor t 
        let before = state' ^? playerStateAccessor t . ix tc
        let message s = printf
                        "attack: %d\ndefense: %d\nbefore: %s\nexpected: %s\nresult: %s\n"
                        attack' defense' (show before) (show (expected ^? ix tc)) s
        case test of
             Left s -> return Prop.failed {Prop.reason = s}
             Right result -> if (result ^. playerStateAccessor t) == expected
                then return Prop.succeeded
                else return Prop.failed {Prop.reason = message $ show (result ^? playerStateAccessor t . ix tc)}


    prop "execAction Defense" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let target' = TargetCard p c
        let state' = initializeBattleState setting'
        let properties' = ((setting' ^. (playerAccessor p)) !! c ^. properties)
        let defense' = properties' ^. defense
        let (Right afterState) = runExecActionTest setting' state' p c Defense target'
        let (Right afterProp) = runCurrentPropertiesTest setting' afterState p c
        let expected = properties' & defense %~ (+ defense')
        let message = printf
                        "defense: %d\nbefore: %s\nexpected: %s\nresult: %s\n"
                        defense' (show properties') (show expected) (show afterProp)
        if afterProp == expected
            then return Prop.succeeded
            else return Prop.failed {Prop.reason = message}

runCurrentPropertiesTest :: BattleSetting -> BattleState -> Player -> Int -> Either String PropertySet
runCurrentPropertiesTest e s p c = result
    where m = currentProperties p c
          (result, _, _) = runRWS (runErrorT m) e s

runExecActionTest ::BattleSetting -> BattleState -> Player -> Int -> Action -> Target -> Either String BattleState
runExecActionTest e s p c a t = result
    where m = execAction p c a t >> get
          (result, _, _) = runRWS (runErrorT m) e s
