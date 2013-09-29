module Battle.Battle where

import Battle.Types
import Battle.IO
import Battle.Target
import Battle.Action

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import Data.List (sortBy)
import qualified Data.Ord as Ord (compare)
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
    put ((BattleSetting fcs scs turns), initializeBattleState (BattleSetting fcs scs turns))
    whileM_ isRunning battleTurn

initializeBattleState :: BattleSetting -> BattleState
initializeBattleState s = BattleState (initCards $ s ^. firstCards) (initCards $ s ^. secondCards) [] (s ^. maxTurn)
    where maxHp' x = x ^. properties . maxHp
          maxMp' x = x ^. properties . maxMp
          initCards = map (\x -> CardState (maxHp' x) (maxMp' x))

isRunning :: BattleMachine Bool
isRunning = do
    (_, s) <- get
    return $ firstAlive s && secondAlive s && turnRemain s
    where firstAlive x = (foldl cardAlive 0 (x ^. first)) > 0
          secondAlive x = (foldl cardAlive 0 (x ^. second)) > 0
          cardAlive x c = if (c ^. hp) > 0 then x + 1 else x
          turnRemain x = case (x ^. remainingTurn) of
                              Nothing -> True
                              (Just n) -> n > 0

battleTurn = return ()
{-
battleTurn :: BattleMachine ()
battleTurn = do
    xs <- inputFirstPlayerCommands
    ys <- inputSecondPlayerCommands
    (settings, state) <- get
    let commands = sortBy (battleCommandCompare settings (state ^. effects)) ((f FirstPlayer xs) ++ (f SecondPlayer ys))
    let (_, s, w) = runRWS (execTurn commands) settings state
    outputBattleState s
        where f p zs = map (\c -> (BattleCommand p c)) zs
-}

battleCommandCompare :: BattleSetting -> [BattleEffect] -> BattleCommand -> BattleCommand -> Ordering
battleCommandCompare s es l r =
    if la /= ra
        then compare la ra
        else compare ls rs
        where la = l ^. action
              ra = r ^. action
              lp = l ^. player
              rp = r ^. player
              lc = l ^. card
              rc = r ^. card
              ls = (currentProperties s es lp lc) ^. speed
              rs = (currentProperties s es rp rc) ^. speed

execTurn :: [BattleCommand] -> BattleTurn ()
execTurn cs = forM cs execCommand >> consumeTurn >> cutoffHpMp

execCommand :: BattleCommand -> BattleTurn ()
execCommand _ = return ()

consumeTurn :: BattleTurn ()
consumeTurn = do
    s <- get
    let es = s ^. effects
    let consumed = map consume es
    let filtered = filter activeEffect consumed
    put $ (s & effects .~ filtered) & remainingTurn %~ (fmap (1-))
        where consume e = e & remaining %~ (fmap (1-))
              activeEffect e = case (e ^. remaining) of
                                    Nothing -> True
                                    (Just n) -> n > 0

cutoffHpMp :: BattleTurn ()
cutoffHpMp = do
    settings <- ask
    state <- get
    let effects' = state ^. effects
    put $ (state & first %~ (cutoff settings effects' FirstPlayer)) & second %~ (cutoff settings effects' SecondPlayer)
        where cutoff s e p cs = map (cutoffCard s e cs p) [0..((length cs) - 1)]
              cutoffCard s e cs p k = CardState (min (c ^. hp) maxHp') (min (c ^. mp) maxMp')
                where c = cs !! k
                      properties' = currentProperties s e p k
                      maxHp' = properties' ^. maxHp
                      maxMp' = properties' ^. maxMp
