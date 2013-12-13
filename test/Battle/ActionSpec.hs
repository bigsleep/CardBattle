module Battle.ActionSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Test.QuickCheck
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
        let expected = ((e ^. (playerAccessor p)) !! c ^. properties)
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest e s p c expected message


    prop "currentProperties 攻撃力2倍2倍" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let u = factorDenominator
        let unitFactor = PropertySet u u u u u u
        let factor = unitFactor & attack .~ (2 * u)
        let doubleAttack = (BattleEffect TargetAll factor, 1)
        let effects' = [doubleAttack, doubleAttack]
        let state' = (initializeBattleState setting') & effects .~ effects'
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = before `applyPropertyFactor` (factor `applyPropertyFactor` factor)
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message


    prop "currentProperties 攻撃力1/2*1/2" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let u = factorDenominator
        let unitFactor = PropertySet u u u u u u
        let factor = unitFactor & attack .~ (u `div` 2)
        let halfAttack = (BattleEffect TargetAll factor, 1)
        let effects' = [halfAttack, halfAttack]
        let state' = (initializeBattleState setting') & effects .~ effects'
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = before & attack %~ (`div` 4)
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message


    prop "currentProperties エフェクトランダム" $ \setting' effects' p -> do
        c <- chooseTargetCard setting' p
        let state' = (initializeBattleState setting') & effects .~ effects'
        let es = map (^. _1 . factor) $ filter ((onTarget p c) . (^. _1 . target)) effects'
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = foldl applyPropertyFactor before es
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message


    prop "currentProperties エフェクト対象外" $ \setting' effect' p -> do
        c <- chooseTargetCard setting' p
        tc <- chooseTargetCard setting' (opponentPlayer p)
        let target' = TargetCard (opponentPlayer p) tc
        let state' = (initializeBattleState setting') & effects .~ [(effect' & _1 . target .~ target')]
        let expected = ((setting' ^. (playerAccessor p)) !! c ^. properties)
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message

    
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
        let test = runExecActionTest setting' state' p c (Attack 100) target'
        let expectedState = state' & playerAccessor t . ix tc . hp .~ (hp' - damage)
        let expected = expectedState ^. playerAccessor t 
        let before = state' ^? playerAccessor t . ix tc
        let message s = printf
                        "attack: %d\ndefense: %d\nbefore: %s\nexpected: %s\nresult: %s\n"
                        attack' defense' (show before) (show (expected ^? ix tc)) s
        case test of
             Left s -> return Prop.failed {Prop.reason = s}
             Right result -> if (result ^. playerAccessor t) == expected
                then return Prop.succeeded
                else return Prop.failed {Prop.reason = message $ show (result ^? playerAccessor t . ix tc)}


    prop "execAction Defense" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let target' = TargetCard p c
        let state' = initializeBattleState setting'
        let properties' = ((setting' ^. (playerAccessor p)) !! c ^. properties)
        let defense' = properties' ^. defense
        let (Right afterState) = runExecActionTest setting' state' p c (Defense 200) target'
        let expected = properties' & defense %~ (+ defense')
        let message result = printf
                        "defense: %d\nbefore: %s\nexpected: %s\nafterState: %s\nresultProperty: %s"
                        defense' (show properties') (show expected) (show afterState) (show result)
        return $ runCurrentPropertiesTest setting' afterState p c expected message


runCurrentPropertiesTest :: BattleSetting -> BattleState -> Player -> Int -> PropertySet -> (PropertySet -> String) -> Prop.Result
runCurrentPropertiesTest e s p c expected message = mkResult r
    where m = currentProperties p c
          (r, _, _) = runRWS (runErrorT m) e s
          mkResult (Left s) = Prop.failed {Prop.reason = s}
          mkResult (Right result) = if expected == result
                                       then Prop.succeeded
                                       else Prop.failed {Prop.reason = message result}

runExecActionTest :: BattleSetting -> BattleState -> Player -> Int -> Action -> Target -> Either String BattleState
runExecActionTest e s p c a t = result
    where m = execAction p c a t >> get
          (result, _, _) = runRWS (runErrorT m) e s
