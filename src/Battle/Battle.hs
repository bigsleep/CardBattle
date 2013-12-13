module Battle.Battle
    ( battle
    , initializeBattleState
    ) where

import Battle.Types
import Battle.IO
import Battle.Target
import Battle.Action
import qualified Battle.TargetCapacity as TC

import Prelude hiding (lookup)
import Data.Functor ()
import Data.Maybe ()
import GHC.Exts(sortWith)
import Control.Lens hiding (Action)
import Control.Monad (forM, forM_)
import Control.Monad.Error (runErrorT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State ()
import Control.Monad.State.Class (get, put)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Trans.RWS (runRWS)
import Control.Monad.Free ()
import Control.Monad.Loops (whileM_)

battle :: BattleMachine ()
battle = do
    setting' <- loadSetting
    put (setting', initializeBattleState setting')
    whileM_ isRunning battleTurn

initializeBattleState :: BattleSetting -> BattleState
initializeBattleState s = BattleState (initCards $ s ^. first) (initCards $ s ^. second) [] [] 0
    where maxHp' x = x ^. properties . maxHp
          maxMp' x = x ^. properties . maxMp
          initCards = map (\x -> CardState (maxHp' x) (maxMp' x))

isRunning :: BattleMachine Bool
isRunning = do
    (e, s) <- get
    return $ firstAlive s && secondAlive s && turnRemain e s
    where firstAlive x = (foldl cardAlive 0 (x ^. first)) > 0
          secondAlive x = (foldl cardAlive 0 (x ^. second)) > 0
          cardAlive x c = if (c ^. hp) > 0 then x + 1 else x
          turnRemain e s = case e ^. maxTurn of
                              Nothing -> True
                              Just n -> s ^. turn < n

toBattleMachine :: BattleTurn a -> BattleMachine a
toBattleMachine x = do
    (setting', state') <- get
    let (r, s, l) = runRWS (runErrorT x) setting' state'
    case r of
        Left m -> outputError m
        Right m -> do
            put (setting', s)
            tell [BattleLog s l []]
            return m

sortBattleCommands :: [BattleCommand] -> BattleTurn [BattleCommand]
sortBattleCommands cs = do
    speedFactors <- forM cs currentSpeed
    let zipped = zip speedFactors cs
    return $ map snd (sortWith fst zipped)
    where currentSpeed (BattleCommand p c a _) = currentProperties p c >>= \q -> return (a, q ^. speed)

toBattleCommand :: Player -> PlayerCommand -> BattleMachine BattleCommand
toBattleCommand p (PlayerCommand c s t)  = do
    (setting', state') <- get
    card' <- fromJust $ setting' ^? (playerAccessor p) . ix c
    Skill a tc <- fromJust $ card' ^? skills . ix s
    let targets = enumerateTargets setting' state' p c (targetable tc)
    target' <- fromJust $ targets ^? ix t
    return $ BattleCommand p c a target'
    where fromJust (Just a) = return a
          fromJust Nothing = outputError "in toBattleCommand. fromJust: Nothing"

battleTurn :: BattleMachine ()
battleTurn = do
    t <- (get >>= return . (^. _2 . turn))
    xs <- (enumerateCommandChoice FirstPlayer) >>= inputPlayerCommands t FirstPlayer
    ys <- (enumerateCommandChoice SecondPlayer) >>= inputPlayerCommands t SecondPlayer
    xs' <- mapM (toBattleCommand FirstPlayer) xs
    ys' <- mapM (toBattleCommand SecondPlayer) ys
    toBattleMachine $ execTurn (xs' ++ ys')
    consumeTurn
    cutoffHpMp

execTurn :: [BattleCommand] -> BattleTurn ()
execTurn cs = do
    sorted <- sortBattleCommands cs
    forM_ sorted execCommand
    s <- get
    put $ s & oneTurnEffects .~ []

execCommand :: BattleCommand -> BattleTurn ()
execCommand (BattleCommand p c a t) = do
    state' <- get
    card' <- getCardState state'
    if canPerform card' a
        then execAction p c a t
        else tell [BattleCommandLog (BattleCommand p c a t) [Underqualified]]
    where getCardState s = case s ^? playerAccessor p . ix c of
                                Nothing -> throwError "in execCommand."
                                Just x -> return x

cutoffHpMp :: BattleMachine ()
cutoffHpMp = do
    (setting', state') <- get
    firstProp <- getProperties setting' state' FirstPlayer
    secondProp <- getProperties setting' state' SecondPlayer
    let firstCutoff = map cutoff ((state' ^. first) `zip` firstProp)
    let secondCutoff = map cutoff ((state' ^. second) `zip` secondProp)
    put $ (setting', (state' & first .~ firstCutoff) & second .~ secondCutoff)
    where getProperties e s p = case rs of
                                     Nothing -> outputError "in cutoffHpMp."
                                     Just x -> return x
            where rs = forM cards (currentProperties' e s p)
                  cards = [0..(cardNum - 1)]
                  cardNum = length $ e ^. playerAccessor p
          cutoff (s, p) = CardState (min (s ^. hp) (p ^. maxHp)) (min (s ^. mp) (p ^. maxMp))

consumeTurn :: BattleMachine ()
consumeTurn = do
    (e, s) <- get
    let consumed = map consume (s ^. effects)
    let filtered' = filter activeEffect consumed
    put $ (e, (s & effects .~ filtered') & turn %~ (+1))
        where consume (e, n) = (e, n - 1)
              activeEffect e = e ^. _2 > 0

enumerateCommandChoice :: Player -> BattleMachine [CommandChoice]
enumerateCommandChoice p = do
    (setting', state') <- get
    let cards = setting' ^. playerAccessor p
    let cardStates = state' ^. playerAccessor p
    let cardNum = length cards
    let withIndex = zip3 [0..(cardNum - 1)] cards cardStates
    mapM applyCard . filter active $ withIndex
    where active (_, _, x) = x ^. hp > 0
          applyCard (i, x, y) = enumerateActionChoice p i x y >>= \ac -> return $ CommandChoice i ac

enumerateActionChoice :: Player -> Int -> Card -> CardState -> BattleMachine [ActionChoice]
enumerateActionChoice p c q s = do
    (setting', state') <- get
    return $ actionChoices setting' state'
    where executables = filter (canPerform s . (^. action)) (q ^. skills)
          withIndex = zip [0..(length executables - 1)] executables
          actionChoices e a = map (apply e a) withIndex
          apply e x (i, (Skill a t)) = ActionChoice i a (enumerateTargets e x p c (targetable t)) 

