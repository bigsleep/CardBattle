{-# LANGUAGE FlexibleContexts  #-}
module Battle.Action
    ( execAction
    , canPerform
    , isCardAlive
    , getCardState
    ) where

import qualified Battle.Types as T
import qualified Battle.Target as Target
import qualified Battle.Property as P

import Prelude hiding (lookup)
import Control.Monad (forM, filterM)
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Lens ((^.), (&), (%~), (.~), (^?), ix)

execAction :: T.Player -> Int -> T.Action -> T.Target -> T.BattleTurn ()

-- Attack
execAction p c (T.Attack f) t = do
    setting' <- ask
    prop <- P.currentProperties p c
    let attack' = (prop ^. T.attack * f) `div` P.factorDenominator
    let ts = Target.enumerateAsCards setting' t
    logs <- forM ts (attackOne attack')
    tell [T.BattleCommandLog (T.BattleCommand p c (T.Attack f) t) (failureIfEmpty logs)]
    
-- Defense
execAction p c (T.Defense f) t = do
    setting <- ask
    state <- get
    ts <- filterM (g . isCardAlive state) (Target.enumerateAsCards setting t)
    let effects = map effect ts
    if null effects
        then tell [T.BattleCommandLog (T.BattleCommand p c (T.Defense f) t) [T.ActionFailure]]
        else do
            let changes = map change effects
            put $ state & T.oneTurnEffects %~ (++ effects)
            tell [T.BattleCommandLog (T.BattleCommand p c (T.Defense f) t) changes]
    where effect x = T.BattleEffect x T.DefenseTag f
          change x = T.PropertyChange (x ^. T.target) (x ^. T.property) (x ^. T.factor)
          g Nothing = throwError "in execAction."
          g (Just x) = return x

-- Heal
execAction p c (T.Heal a b) t = do
    setting' <- ask
    let ts = Target.enumerateAsCards setting' t
    logs <- forM ts (healOne a)
    l <- consumeMp p c b
    tell [T.BattleCommandLog (T.BattleCommand p c (T.Heal a b) t) (l : logs)]

-- Buff
execAction p c (T.Buff q f a b) t = do
    setting <- ask
    state <- get
    cs <- filterM (alive state) (Target.enumerateAsCards setting t)
    let effects = map (\x -> (T.BattleEffect x q f, a)) cs
    if null effects
        then tell [T.BattleCommandLog (T.BattleCommand p c (T.Buff q f a b) t) [T.ActionFailure]]
        else do
            let changes = map (\x -> T.PropertyChange x q f) cs
            put $ state & T.effects %~ (++ effects)
            l <- consumeMp p c b
            tell [T.BattleCommandLog (T.BattleCommand p c (T.Buff q f a b) t) (l : changes)]
    where alive s (tp, tc) = case s ^? T.playerAccessor tp . ix tc of
                                  Just cstate -> return $ cstate ^. T.hp > 0
                                  Nothing -> throwError "in execAction. list index out of range."


-- 単体攻撃
attackOne :: Int -> (T.Player, Int) -> T.BattleTurn T.ActionResult
attackOne attack' (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (T.playerAccessor tp . ix tc))
    if hp' <= 0
        then return T.ActionFailure
        else do
            prop <- P.currentProperties tp tc
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
            prop <- P.currentProperties tp tc
            let maxHp' = prop ^. T.maxHp
            let incHp = max 0 (min h (maxHp' - hp'))
            put $ state' & (T.playerAccessor tp . ix tc . T.hp) .~ (hp' + incHp)
            return (T.StateChange (tp, tc) (T.CardState incHp 0))
    where getHp (Just d) = return (d ^. T.hp)
          getHp Nothing = throwError "in healOne. list index out of range."


-- canPerform
canPerform :: T.CardState -> T.Action -> Bool

canPerform _ (T.Attack _) = True

canPerform _ (T.Defense _) = True

canPerform s (T.Heal _ b) = s ^. T.mp >= b

canPerform s (T.Buff _ _ _ b) = s ^. T.mp >= b

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

failureIfEmpty :: [T.ActionResult] -> [T.ActionResult]
failureIfEmpty [] = [T.ActionFailure]
failureIfEmpty as = as

isCardAlive :: T.BattleState -> (T.Player, Int) -> Maybe Bool
isCardAlive s (p, c) = do
    cardState <- getCardState s (p, c)
    return $ cardState ^. T.hp > 0

getCardState :: T.BattleState -> (T.Player, Int) -> Maybe T.CardState
getCardState s (p, c) = s ^? T.playerAccessor p . ix c
