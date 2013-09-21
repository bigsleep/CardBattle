module Battle.Battle where

import Battle.Types

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import Data.Map (Map, lookup, findWithDefault, adjust)
import Data.Set hiding (map, filter, foldl, insert)
import Control.Lens hiding (Action)
import Control.Monad(forM, when)
import Control.Monad.State()
import Control.Monad.State.Class(get, put)
import Control.Monad.Reader.Class(ask)
import Control.Monad.Trans.RWS(RWS)
import Control.Monad.Free()
import System.Random()

currentProperty :: PlayerTag -> CardPosition -> BattleSetting -> [BattleEffect] -> PropertySet
currentProperty p c s e = applyEffect card
    where card = lookup c (s ^. (playerAccessor p))
          applyEffect Nothing = PropertySet 0 0 0 0 0 0
          applyEffect (Just (Card q _)) = eff q
          eff = foldl (.) id effectFunctions
          effectFunctions = map (^. effect) $ filter ((onTarget p c) . (^. target)) e

type BattleTurn = RWS BattleSetting [String] BattleState ()

execTurn :: [BattleCommand] -> BattleTurn
execTurn cs = forM cs execCommand >> return ()

execCommand :: BattleCommand -> BattleTurn
execCommand (BattleCommand p (PlayerCommand c a)) = execCommand' p c a

execCommand' :: PlayerTag -> CardPosition -> Action -> BattleTurn
execCommand' p c a = return ()


main = putStrLn "hello"

