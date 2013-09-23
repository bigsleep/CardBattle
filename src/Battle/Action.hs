module Battle.Action where

import Battle.Types

import Prelude hiding (lookup)
import qualified Data.Map as M (adjust, lookup, (!))
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Lens hiding (Action)

currentProperties :: PlayerTag -> CardPosition -> BattleSetting -> [BattleEffect] -> PropertySet
currentProperties p c s e = applyEffect card
    where card = (s ^. (playerAccessor p)) M.! c
          applyEffect (Card q) = eff q
          eff = foldl (.) id effectFunctions
          effectFunctions = map (^. effect) $ filter ((onTarget p c) . (^. target)) e

canPerform :: CardState -> Action -> Bool
execAction :: PlayerTag -> CardPosition -> Action -> BattleTurn

canPerform _ (Attack _) = True
canPerform _ Defense = True
canPerform c (Heal a b _) = (c ^. mp) >= a

execAction p c (Attack (TargetCard tp tc)) = do
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
