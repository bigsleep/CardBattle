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

{-
setHp :: Player -> Int -> BattleTurn ()
setHp p c =

execAction :: Player -> Int -> Action -> Target -> BattleTurn ()

execAction p c Attack (TargetCard tp tc) = do
    settings <- ask
    state <- get
    let effects' = state ^. effects
    let attack' = (currentProperties p c settings effects') ^. attack
    let defense' = (currentProperties tp tc settings effects') ^. defense
    let hp' = ((state ^. playerStateAccessor tp) M.! tc) ^. hp
    let damage = min (attack' - defense') hp'
    put $ state & (playerStateAccessor tp) %~ (updateCard (decreaseHp damage) tc)
        where updateCard f k m = M.adjust f k m
              decreaseHp d x = x {_hp = (x ^. hp - d)}

execAction p c Defense = do
    settings <- ask
    state <- get
    let defense' = ((settings ^. playerAccessor p) M.! c) ^. (properties . defense)
    let eff = (BattleEffect (Target (TargetCard p c)) (boostDefense defense') (Just 1))
    put $ state & effects %~ (eff :)
        where boostDefense d p = p & defense %~ (+d)

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


