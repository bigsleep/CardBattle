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

subPropertySet :: PropertySet -> PropertySet -> PropertySet
subPropertySet (PropertySet a b c d e f) (PropertySet a' b' c' d' e' f') = PropertySet (a - a') (b - b') (c - c') (d - d') (e - e') (f - f')

currentProperties :: Player -> Int -> BattleTurn PropertySet
currentProperties p c = do
    e <- ask
    s <- get
    case (currentProperties' e s p c) of
         Nothing -> throwError "in currentProperties. list index out of range."
         Just x  -> return x

currentProperties' :: BattleSetting -> BattleState -> Player -> Int -> Maybe PropertySet
currentProperties' e s p c = fmap applyEffect card'
    where card' = (e ^. (playerAccessor p)) ^? ix c
          applyEffect (Card _ q _) = applyPropertyFactor q eff
          eff = foldl multPropertyFactor unitPropertyFactor (effectFactors (s ^. oneTurnEffects ++ s ^. effects))
          effectFactors x = map (^. factor) $ filter ((onTarget p c) . (^. target)) x

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
        then return (StateChange (tp, tc) (CardState 0 0))
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
        then return (StateChange (tp, tc) (CardState 0 0))
        else do
            prop <- currentProperties tp tc
            let maxHp' = prop ^. maxHp
            let incHp = max 0 (min h (maxHp' - hp'))
            put $ state' & (playerAccessor tp . ix tc . hp) .~ (hp' + incHp)
            return (StateChange (tp, tc) (CardState incHp 0))
    where getHp (Just d) = return (d ^. hp)
          getHp Nothing = throwError "in healOne. list index out of range."


-- canPerform
canPerform :: CardState -> Skill -> Bool

canPerform _ (Skill Attack _) = True

canPerform _ (Skill Defense _) = True

canPerform s (Skill (Heal a b) _) = s ^. mp >= b

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

