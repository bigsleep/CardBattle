module Battle.Action where

import Battle.Types
import Battle.IO
import Battle.Target

import Prelude hiding (lookup)
import qualified Data.List as L (find)
import qualified Data.Map as M (adjust, lookup, (!))
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Lens hiding (Action)

execAction :: Player -> Int -> Action -> Target -> BattleTurn ()

-- Attack
execAction p c Attack t = do
    setting' <- ask
    prop <- currentProperties p c
    let attack' = prop ^. attack
    let ts = enumerateAsCards setting' t
    logs <- forM ts (attackOne attack')
    tell [BattleCommandLog (BattleCommand p c Attack t) logs]
    
-- Defense
execAction p c Defense t = do
    setting' <- ask
    state' <- get
    let effect' = BattleEffect Defense t (unitPropertyFactor & defense .~ 2) (Just 1)
    let ts = enumerateAsCards setting' t
    before <- getProperties ts
    put $ state' & oneTurnEffects %~ (effect' :)
    after <- getProperties ts
    let changes = map (\(tc, (x, y)) -> PropertyChange tc (y `subPropertySet` x)) (ts `zip` (before `zip` after))
    tell [BattleCommandLog (BattleCommand p c Defense t) changes]
    where getProperties xs = forM xs (uncurry currentProperties)

-- Heal
execAction p c (Heal a b) t = do
    setting' <- ask
    let ts = enumerateAsCards setting' t
    logs <- forM ts (healOne a)
    l <- consumeMp p c b
    tell [BattleCommandLog (BattleCommand p c (Heal a b) t) (l : logs)]

-- Buff
execAction p c (Buff q f a b) t = do
    setting' <- ask
    state' <- get
    let factor = unitPropertyFactor & propertyAccessor q .~ f
    let effect' = BattleEffect (Buff q f a b) t factor (Just a)
    let ts = enumerateAsCards setting' t
    before <- getProperties ts
    put $ state' & effects %~ (effect' :)
    after <- getProperties ts
    l <- consumeMp p c b
    let changes = map (\(tc, (x, y)) -> PropertyChange tc (y `subPropertySet` x)) (ts `zip` (before `zip` after))
    tell [BattleCommandLog (BattleCommand p c (Buff q f a b) t) (l : changes)]
    where getProperties xs = forM xs (uncurry currentProperties)


-- 単体攻撃
attackOne :: Int -> (Player, Int) -> BattleTurn ActionResult
attackOne attack' (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (playerAccessor tp . ix tc))
    if hp' <= 0
        then return ActionFailure
        else do
            prop <- currentProperties tp tc
            let defense' = prop ^. defense
            let damage = min (max 1 (attack' - defense')) hp'
            put $ state' & (playerAccessor tp . ix tc . hp) .~ (hp' - damage)
            return (StateChange (tp, tc) (CardState (- damage) 0))
    where getHp (Just d) = return (d ^. hp)
          getHp Nothing = throwError "in attackOne. list index out of range."


-- 単体回復
healOne :: Int -> (Player, Int) -> BattleTurn ActionResult
healOne h (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (playerAccessor tp . ix tc))
    if hp' <= 0
        then return ActionFailure
        else do
            prop <- currentProperties tp tc
            let maxHp' = prop ^. maxHp
            let incHp = max 0 (min h (maxHp' - hp'))
            put $ state' & (playerAccessor tp . ix tc . hp) .~ (hp' + incHp)
            return (StateChange (tp, tc) (CardState incHp 0))
    where getHp (Just d) = return (d ^. hp)
          getHp Nothing = throwError "in healOne. list index out of range."


-- canPerform
canPerform :: CardState -> Action -> Bool

canPerform _ Attack = True

canPerform _ Defense = True

canPerform s (Heal a b) = s ^. mp >= b

canPerform s (Buff q f a b) = s ^. mp >= b

consumeMp :: Player -> Int -> Int -> BattleTurn ActionResult
consumeMp p c q = do
    s <- get
    mp' <- getMp s
    if mp' >= q
        then put $ s & playerAccessor p . ix c . mp .~ (mp' - q)
        else throwError "in consumeMp. mp less than consumption."
    return $ Consume (p, c) (CardState 0 q)
    where getMp x = case x ^? playerAccessor p . ix c . mp of
                         Nothing -> throwError "in consumeMp."
                         Just y -> return y

