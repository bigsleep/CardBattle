module Battle.ActionSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck.Property as Prop (Result(..), succeeded, failed)
import Control.Monad.State (get)
import Control.Monad.RWS (runRWS)
import Control.Monad.Error (runErrorT)
import Control.Lens ((^.), (.~), (%~), (^?), (&), ix, _1)
import Text.Printf (printf)

import Battle.Types
import Battle.Property
import Battle.Battle
import Battle.Action
import Battle.TestUtil (chooseTargetCard)

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
        let factor' = unitFactor & attack .~ (2 * u)
        let doubleAttack = (BattleEffect (p, c) AttackTag (u * 2), 1)
        let effects' = [doubleAttack, doubleAttack]
        let state' = (initializeBattleState setting') & effects .~ effects'
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = before `applyPropertyFactor` factor' `applyPropertyFactor` factor'
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message


    prop "currentProperties 攻撃力1/2*1/2" $ \setting' p -> do
        c <- chooseTargetCard setting' p
        let u = factorDenominator
        let halfAttack = (BattleEffect (p, c) AttackTag (u `div` 2), 1)
        let effects' = [halfAttack, halfAttack]
        let state' = (initializeBattleState setting') & effects .~ effects'
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = before & attack %~ (`div` 4)
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message


    prop "currentProperties エフェクトランダム" $ \setting' effects' p -> do
        c <- chooseTargetCard setting' p
        let state' = (initializeBattleState setting') & effects .~ effects'
        let targeted q d e = (q, d) == e ^. _1 . target
        let es = map (toPropertyFactor . (^. _1)) $ filter (targeted p c) effects'
        let before = (setting' ^. (playerAccessor p)) !! c ^. properties
        let expected = foldl applyPropertyFactor before es
        let message r = "result: " ++ show r ++ "\nexpected: " ++ show expected
        return $ runCurrentPropertiesTest setting' state' p c expected message


    prop "currentProperties エフェクト対象外" $ \setting' effect' p -> do
        c <- chooseTargetCard setting' p
        tc <- chooseTargetCard setting' (opponentPlayer p)
        let target' = (opponentPlayer p, tc)
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
          mkResult (Left err) = Prop.failed {Prop.reason = err}
          mkResult (Right result) = if expected == result
                                       then Prop.succeeded
                                       else Prop.failed {Prop.reason = message result}

runExecActionTest :: BattleSetting -> BattleState -> Player -> Int -> Action -> Target -> Either String BattleState
runExecActionTest e s p c a t = result
    where m = execAction p c a t >> get
          (result, _, _) = runRWS (runErrorT m) e s
