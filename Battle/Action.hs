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
          applyEffect (Card q _) = eff q
          eff = foldl (.) id effectFunctions
          effectFunctions = map (^. effect) $ filter ((onTarget p c) . (^. target)) e

execAction :: PlayerTag -> CardPosition -> Action -> BattleTurn

-- Attack
execAction p c (Attack (TargetCard tp tc)) = do
    settings <- ask
    state <- get
    let effects' = state ^. effects
    let attack' = (currentProperties p c settings effects') ^. attack
    let defense' = (currentProperties tp tc settings effects') ^. defense
    let hp' = ((state ^. playerStateAccessor tp) M.! tc) ^. hp
    let damage = min (attack' - defense') hp'
    put $ state & (playerStateAccessor tp) %~ (inflictDamage tc damage)
        where inflictDamage k d m = M.adjust (decreaseHp d) k m
              decreaseHp d x = CardState (x ^. hp - d) (x ^. mp)

-- Defense
execAction p c Defense = do
    settings <- ask
    state <- get
    let defense' = ((settings ^. playerAccessor p) M.! c) ^. (properties . defense)
    let eff = (BattleEffect (Target (TargetCard p c)) (boostDefense defense') (Just 1))
    put $ state & effects %~ (eff :)
        where boostDefense d p = p & defense %~ (+d)
