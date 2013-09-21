module Battle.Battle where

import Battle.Types
import Battle.IO

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import qualified Data.Map as M (Map, lookup, findWithDefault, adjust, map)
import qualified Data.Set as Set hiding (map, filter, foldl, insert)
import Control.Lens hiding (Action)
import Control.Monad (forM, when)
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.RWS (RWS)
import Control.Monad.Free ()
import Control.Monad.Loops (whileM_)

battle :: BattleMachine ()
battle = do
    turns <- loadSetting
    fcs <- loadFirstPlayerCards
    scs <- loadSecondPlayerCards
    s <- get
    put ((BattleSetting fcs scs turns), initializeBattleState fcs scs turns)
    case turns of
         Nothing -> whileM_ bothAlive battleTurn
         (Just n) -> replicateM_ n battleTurn
    return ()
        where initializeBattleState x y ts = BattleState (initCards x) (initCards y) [] ts
              maxHp' x = x ^. properties . maxHp
              maxMp' x = x ^. properties . maxMp
              initCards c = M.map (\x -> CardState (maxHp' x) (maxMp' x)) c

battleTurn :: BattleMachine ()
battleTurn = do
    xs <- inputFirstPlayerCommands
    ys <- inputSecondPlayerCommands
    let commands = sortBattleCommands $ (f xs) ++ (f ys)
    (settings, state) <- get
    let (_, s, w) = runRWS (execTurn commands) settings state
    outputBattleState s
    outputBattleLog w

type BattleTurn = RWS BattleSetting [String] BattleState ()

execTurn :: [BattleCommand] -> BattleTurn
execTurn cs = forM cs execCommand >> consumeTurn >> cutoffHpMp

execCommand :: BattleCommand -> BattleTurn
execCommand (BattleCommand p (PlayerCommand c a)) = execCommand' p c a

execCommand' :: PlayerTag -> CardPosition -> Action -> BattleTurn
execCommand' p c a = return ()

currentProperties :: PlayerTag -> CardPosition -> BattleSetting -> [BattleEffect] -> PropertySet
currentProperties p c s e = applyEffect card
    where card = M.lookup c (s ^. (playerAccessor p))
          applyEffect Nothing = PropertySet 0 0 0 0 0 0
          applyEffect (Just (Card q _)) = eff q
          eff = foldl (.) id effectFunctions
          effectFunctions = map (^. effect) $ filter ((onTarget p c) . (^. target)) e
