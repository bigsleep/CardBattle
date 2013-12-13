module Battle.Action
    ( execAction
    , canPerform
    , currentProperties
    , currentProperties'
    ) where

import qualified Battle.Types as T
import qualified Battle.Target as Target

import Prelude hiding (lookup)
import Control.Monad (forM)
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Error
import Control.Monad.Error.Class ()
import Control.Lens hiding (Action)

execAction :: T.Player -> Int -> T.Action -> T.Target -> T.BattleTurn ()

-- Attack
execAction p c (T.Attack f) t = do
    setting' <- ask
    prop <- currentProperties p c
    let attack' = (prop ^. T.attack * f) `div` T.factorDenominator
    let ts = Target.enumerateAsCards setting' t
    logs <- forM ts (attackOne attack')
    tell [T.BattleCommandLog (T.BattleCommand p c (T.Attack f) t) logs]
    
-- Defense
execAction p c (T.Defense f) t = do
    setting' <- ask
    state' <- get
    let effect' = T.BattleEffect t (unitFactor & T.defense .~ f)
    let ts = Target.enumerateAsCards setting' t
    before <- getProperties ts
    put $ state' & T.oneTurnEffects %~ (effect' :)
    after <- getProperties ts
    let changes = map (\(tc, (x, y)) -> T.PropertyChange tc (y `T.subPropertySet` x)) (ts `zip` (before `zip` after))
    tell [T.BattleCommandLog (T.BattleCommand p c (T.Defense f) t) changes]
    where unitFactor = T.PropertySet a a a a a a
          a = T.factorDenominator
          getProperties xs = forM xs (uncurry currentProperties)

-- Heal
execAction p c (T.Heal a b) t = do
    setting' <- ask
    let ts = Target.enumerateAsCards setting' t
    logs <- forM ts (healOne a)
    l <- consumeMp p c b
    tell [T.BattleCommandLog (T.BattleCommand p c (T.Heal a b) t) (l : logs)]

-- Buff
execAction p c (T.Buff q f _ b) t = do
    setting' <- ask
    state' <- get
    let factor = unitFactor & T.propertyAccessor q .~ f
    let effect' = (T.BattleEffect t factor, a)
    let ts = Target.enumerateAsCards setting' t
    before <- getProperties ts
    put $ state' & T.effects %~ (effect' :)
    after <- getProperties ts
    l <- consumeMp p c b
    let changes = map (\(tc, (x, y)) -> T.PropertyChange tc (y `T.subPropertySet` x)) (ts `zip` (before `zip` after))
    tell [T.BattleCommandLog (T.BattleCommand p c (T.Buff q f a b) t) (l : changes)]
    where unitFactor = T.PropertySet a a a a a a
          a = T.factorDenominator
          getProperties xs = forM xs (uncurry currentProperties)


-- 単体攻撃
attackOne :: Int -> (T.Player, Int) -> T.BattleTurn T.ActionResult
attackOne attack' (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (T.playerAccessor tp . ix tc))
    if hp' <= 0
        then return T.ActionFailure
        else do
            prop <- currentProperties tp tc
            let defense' = prop ^. T.defense
            let damage = min (max 1 (attack' - defense')) hp'
            put $ state' & (T.playerAccessor tp . ix tc . T.hp) .~ (hp' - damage)
            return (T.StateChange (tp, tc) (T.CardState (- damage) 0))
    where getHp (Just d) = return (d ^. T.hp)
          getHp Nothing = throwError "in attackOne. list index out of range."


-- 単体回復
healOne :: Int -> (T.Player, Int) -> T.BattleTurn T.ActionResult
healOne h (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (T.playerAccessor tp . ix tc))
    if hp' <= 0
        then return T.ActionFailure
        else do
            prop <- currentProperties tp tc
            let maxHp' = prop ^. T.maxHp
            let incHp = max 0 (min h (maxHp' - hp'))
            put $ state' & (T.playerAccessor tp . ix tc . T.hp) .~ (hp' + incHp)
            return (T.StateChange (tp, tc) (T.CardState incHp 0))
    where getHp (Just d) = return (d ^. T.hp)
          getHp Nothing = throwError "in healOne. list index out of range."


-- canPerform
canPerform :: T.CardState -> T.Action -> Bool

canPerform s (T.Attack _) = s ^. T.hp > 0

canPerform s (T.Defense _) = s ^. T.hp > 0

canPerform s (T.Heal _ b) = (s ^. T.hp > 0) && (s ^. T.mp >= b)

canPerform s (T.Buff _ _ _ b) = (s ^. T.hp > 0) && (s ^. T.mp >= b)

consumeMp :: T.Player -> Int -> Int -> T.BattleTurn T.ActionResult
consumeMp p c q = do
    s <- get
    mp' <- getMp s
    if mp' >= q
        then put $ s & T.playerAccessor p . ix c . T.mp .~ (mp' - q)
        else throwError "in consumeMp. mp less than consumption."
    return $ T.Consume (p, c) (T.CardState 0 q)
    where getMp x = case x ^? T.playerAccessor p . ix c . T.mp of
                         Nothing -> throwError "in consumeMp."
                         Just y -> return y

currentProperties :: T.Player -> Int -> T.BattleTurn T.PropertySet
currentProperties p c = do
    e <- ask
    s <- get
    case (currentProperties' e s p c) of
         Nothing -> throwError "in currentProperties. list index out of range."
         Just x  -> return x

currentProperties' :: T.BattleSetting -> T.BattleState -> T.Player -> Int -> Maybe T.PropertySet
currentProperties' e s p c = fmap applyEffect card'
    where card' = (e ^. (T.playerAccessor p)) ^? ix c
          applyEffect (T.Card _ q _) = T.multPropertyFactor q eff
          eff = foldl T.applyPropertyFactor T.unitPropertyFactor (effectFactors (s ^. T.oneTurnEffects ++ (map fst (s ^. T.effects))))
          effectFactors x = map (^. T.factor) $ filter ((T.onTarget p c) . (^. T.target)) x
