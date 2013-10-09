module Battle.Battle where

import Battle.Types
import Battle.IO
import Battle.Target
import Battle.Action

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import GHC.Exts(sortWith)
import qualified Data.Ord as Ord (compare)
import qualified Data.Set as Set hiding (map, filter, foldl, insert)
import Control.Applicative(liftA2)
import Control.Lens hiding (Action)
import Control.Monad (forM, when)
import Control.Monad.Error
import Control.Monad.Error.Class
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

toBattleMachine :: BattleTurn a -> BattleMachine a
toBattleMachine x = do
    (setting', state') <- get
    let (r, s, _) = runRWS (runErrorT x) setting' state'
    case r of
        Left m -> throwError m
        Right m -> put (setting', s) >> return m

sortBattleCommands :: [BattleCommand] -> BattleTurn [BattleCommand]
sortBattleCommands cs = do
    e <- ask
    s <- get
    speedFactors <- forM cs currentSpeed
    let zipped = zip speedFactors cs
    return $ map snd (sortWith fst zipped)
    where currentSpeed (BattleCommand p c a _) = currentProperties p c >>= \p -> return (a, p ^. speed)

toBattleCommand :: Player -> PlayerCommand -> BattleMachine BattleCommand
toBattleCommand p (PlayerCommand c s t)  = do
    (setting', state') <- get
    card' <- fromJust $ setting' ^? (playerAccessor p) . ix c
    Skill a tc <- fromJust $ card' ^? skills . ix s
    let targets = enumerateTargets setting' state' p c (targetable tc)
    target' <- fromJust $ targets ^? ix t
    return $ BattleCommand p c a target'
    where fromJust (Just a) = return a
          fromJust Nothing = throwError "in toBattleCommand. fromJust: Nothing"

battleTurn :: BattleMachine ()
battleTurn = do
    xs <- inputFirstPlayerCommands
    ys <- inputSecondPlayerCommands
    (settings, state) <- get
    xs' <- mapM (toBattleCommand FirstPlayer) xs
    ys' <- mapM (toBattleCommand SecondPlayer) ys
    toBattleMachine $ execTurn (xs' ++ ys')

execTurn cs = return ()
{-
execTurn :: [BattleCommand] -> BattleTurn ()
execTurn cs = do
    sorted <- sortBattleCommands cs
    forM sorted execCommand
    consumeTurn
    cutoffHpMp

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
-}
