module Battle.Action where

import Battle.Types
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

defaultProperties :: Player -> Int -> BattleTurn PropertySet
defaultProperties p c = do
    e <- ask
    case card' e of
         Nothing -> throwError "in defaultProperties. list index out of range."
         Just c  -> return (c ^. properties)
    where card' x = cards x ^? ix c
          cards x = (x ^. (playerAccessor p))

unitPropertyFactor :: PropertyFactor
unitPropertyFactor = PropertyFactor 1 1 1 1 1 1

applyPropertyFactor :: PropertySet -> PropertyFactor -> PropertySet
applyPropertyFactor (PropertySet a1 b1 c1 d1 e1 f1) (PropertyFactor a2 b2 c2 d2 e2 f2) =
    PropertySet (floor $ fromIntegral a1 * a2)
                (floor $ fromIntegral b1 * b2)
                (floor $ fromIntegral c1 * c2)
                (floor $ fromIntegral d1 * d2)
                (floor $ fromIntegral e1 * e2)
                (floor $ fromIntegral f1 * f2)

multPropertyFactor :: PropertyFactor -> PropertyFactor -> PropertyFactor
multPropertyFactor (PropertyFactor a1 b1 c1 d1 e1 f1) (PropertyFactor a2 b2 c2 d2 e2 f2) =
    PropertyFactor (a1 * a2) (b1 * b2) (c1 * c2) (d1 * d2) (e1 * e2) (f1 * f2)

currentProperties :: Player -> Int -> BattleTurn PropertySet
currentProperties p c = do
    e <- ask
    s <- get
    case card' e of
         Nothing -> throwError "in currentProperties. list index out of range."
         Just c  -> return $ applyEffect s c
    where card' x = cards x ^? ix c
          cards x = (x ^. (playerAccessor p))
          applyEffect s (Card _ q _) = applyPropertyFactor q (eff s)
          eff s = foldl multPropertyFactor unitPropertyFactor (effectFactors (s ^. effects))
          effectFactors x = map (^. factor) $ filter ((onTarget p c) . (^. effectTarget)) x

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
    state' <- get
    let effect' = BattleEffect Defense t unitPropertyFactor{_defenseFactor = 2} (Just 1)
    put $ state' & effects %~ (effect' :)

-- Heal
execAction p c (Heal a b) t = do
    setting' <- ask
    let ts = enumerateAsCards setting' t
    logs <- forM ts (healOne a)
    state' <- get
    put $ state' & (playerStateAccessor p . ix c . mp) %~ consumeMp b
    tell [BattleCommandLog (BattleCommand p c (Heal a b) t) (l : logs)]
    where consumeMp x a = max 0 (a - x)
          l = Consume (p, c) (CardState 0 b)


-- 単体攻撃
attackOne :: Int -> (Player, Int) -> BattleTurn ActionResult
attackOne attack' (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (playerStateAccessor tp . ix tc))
    if hp' <= 0
        then return (StateChange (tp, tc) (CardState 0 0))
        else do
            prop <- currentProperties tp tc
            let defense' = prop ^. defense
            let damage = min (max 1 (attack' - defense')) hp'
            put $ state' & (playerStateAccessor tp . ix tc . hp) .~ (hp' - damage)
            return (StateChange (tp, tc) (CardState (- damage) 0))
    where getHp (Just d) = return (d ^. hp)
          getHp Nothing = throwError "in attackOne. list index out of range."


-- 単体回復
healOne :: Int -> (Player, Int) -> BattleTurn ActionResult
healOne h (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (playerStateAccessor tp . ix tc))
    if hp' <= 0
        then return (StateChange (tp, tc) (CardState 0 0))
        else do
            prop <- currentProperties tp tc
            let maxHp' = prop ^. maxHp
            let incHp = max 0 (min h (maxHp' - hp'))
            put $ state' & (playerStateAccessor tp . ix tc . hp) .~ (hp' + incHp)
            return (StateChange (tp, tc) (CardState incHp 0))
    where getHp (Just d) = return (d ^. hp)
          getHp Nothing = throwError "in healOne. list index out of range."
