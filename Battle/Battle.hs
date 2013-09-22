module Battle.Battle where

import Battle.Types
import Battle.IO

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import Data.List (sortBy)
import qualified Data.Ord as Ord (compare)
import qualified Data.Map as M (Map, fold, lookup, findWithDefault, adjust, map)
import qualified Data.Set as Set hiding (map, filter, foldl, insert)
import Control.Lens hiding (Action)
import Control.Monad (forM, when)
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.RWS (RWS, runRWS)
import Control.Monad.Free ()
import Control.Monad.Loops (whileM_)

battle :: BattleMachine ()
battle = do
    turns <- loadSetting
    fcs <- loadFirstPlayerCards
    scs <- loadSecondPlayerCards
    put ((BattleSetting fcs scs turns), initializeBattleState fcs scs turns)
    whileM_ isRunning battleTurn
        where initializeBattleState x y ts = BattleState (initCards x) (initCards y) [] ts
              maxHp' x = x ^. properties . maxHp
              maxMp' x = x ^. properties . maxMp
              initCards c = M.map (\x -> CardState (maxHp' x) (maxMp' x)) c

isRunning :: BattleMachine Bool
isRunning = do
    (_, s) <- get
    return $ firstAlive s && secondAlive s && turnRemain s
    where firstAlive x = (M.fold cardAlive 0 (x ^. first)) > 0
          secondAlive x = (M.fold cardAlive 0 (x ^. second)) > 0
          cardAlive c x = if (c ^. hp) > 0 then x + 1 else x
          turnRemain x = case (x ^. remainingTurn) of
                            Nothing -> True
                            (Just n) -> n > 0

battleTurn :: BattleMachine ()
battleTurn = do
    xs <- inputFirstPlayerCommands
    ys <- inputSecondPlayerCommands
    (settings, state) <- get
    let commands = sortBy (battleCommandCompare settings (state ^. effects)) ((f FirstPlayer xs) ++ (f SecondPlayer ys))
    let (_, s, w) = runRWS (execTurn commands) settings state
    outputBattleState s
        where f p zs = map (\c -> (BattleCommand p c)) zs

battleCommandCompare :: BattleSetting -> [BattleEffect] -> BattleCommand -> BattleCommand -> Ordering
battleCommandCompare st es l r =
    if la /= ra
        then compare la ra
        else compare ls rs
        where la = l ^. command . action
              ra = r ^. command . action
              lp = l ^. player
              rp = l ^. player
              lc = l ^. command . card
              rc = l ^. command . card
              ls = (currentProperties lp lc st es) ^. speed
              rs = (currentProperties rp rc st es) ^. speed

type BattleTurn = RWS BattleSetting [String] BattleState ()

execTurn :: [BattleCommand] -> BattleTurn
--execTurn cs = forM cs execCommand >> consumeTurn >> cutoffHpMp
execTurn cs = return ()

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
