module Battle.Action where

import Battle.Types
import Battle.Target

import Prelude hiding (lookup)
import qualified Data.List as L (find)
import qualified Data.Map as M (adjust, lookup, (!))
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
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

currentProperties :: Player -> Int -> BattleTurn PropertySet
currentProperties p c = do
    e <- ask
    s <- get
    case card' e of
         Nothing -> throwError "in currentProperties. list index out of range."
         Just c  -> return $ applyEffect s c
    where card' x = cards x ^? ix c
          cards x = (x ^. (playerAccessor p))
          applyEffect s (Card q _) = eff s q
          eff s = foldl (.) id (effectFunctions (s ^. effects))
          effectFunctions x = map (^. effect) $ filter ((onTarget p c) . (^. effectTarget)) x


execAction :: Player -> Int -> Action -> Target -> BattleTurn ()

-- Attack
execAction p c Attack (TargetCard tp tc) = do
    setting' <- ask
    state' <- get
    let effects' = state' ^. effects
    prop <- currentProperties p c
    tprop <- currentProperties tp tc
    let attack' = prop ^. attack
    attackOne attack' (tp, tc)

execAction p c Attack t = do
    setting' <- ask
    state' <- get
    let effects' = state' ^. effects
    prop <- currentProperties p c
    let attack' = prop ^. attack
    let ts = enumerateAsCards setting' t
    forM_ ts (attackOne attack')
    
-- Defense
execAction p c Defense t = do
    setting' <- ask
    state' <- get
    prop <- defaultProperties p c
    let defense' = prop ^. defense
    let effect' = BattleEffect t (boostDefense defense') (Just 1)
    put $ state' & effects %~ (effect' :)
        where boostDefense d p = p & defense %~ (+d)

{-
execAction p c (Heal a b (TargetCard tp tc)) = do
    settings <- ask
    state <- get
    let effects' = state ^. effects
    let maxHp' = (currentProperties tp tc settings effects') ^. maxHp
    let hp' = ((state ^. playerStateAccessor tp) M.! tc) ^. hp
    let heal = max 0 (min b (maxHp' - hp'))
    let state' = state & (playerStateAccessor tp) %~ (updateCard (increaseHp heal) tc)
    put $ state' & (playerStateAccessor p) %~ (updateCard (consumeMp a) c)
        where updateCard f k m = M.adjust f k m
              increaseHp d x = x {_hp = (x ^. hp + d)}
              consumeMp q x = x {_mp = (x ^. mp) - q}
-}


attackOne :: Int -> (Player, Int) -> BattleTurn ()
attackOne attack' (tp, tc) = do
    state' <- get 
    hp' <- getHp (state' ^? (playerStateAccessor tp . ix tc))
    if hp' <= 0
        then return ()
        else do
            prop <- currentProperties tp tc
            let defense' = prop ^. defense
            let damage = min (max 1 (attack' - defense')) hp'
            put $ state' & (playerStateAccessor tp . ix tc . hp) .~ (hp' - damage)
    where getHp (Just d) = return (d ^. hp)
          getHp Nothing = throwError "in attackOne. list index out of range."

