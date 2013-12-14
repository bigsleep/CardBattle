module Battle.Battle
    ( battle
    , initializeBattleState
    ) where

import qualified Battle.Types as T
import Battle.IO
import Battle.Target
import Battle.Action

import Prelude hiding (lookup)
import GHC.Exts(sortWith)
import Control.Lens ((^.), (.~), (&), (^?), (%~), ix, _2)
import Control.Monad (forM, forM_, liftM)
import Control.Monad.Error (runErrorT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Class (get, put)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Trans.RWS (runRWS)
import Control.Monad.Loops (whileM_)

battle :: BattleMachine ()
battle = do
    setting' <- loadSetting
    put (setting', initializeBattleState setting')
    whileM_ isRunning battleTurn

initializeBattleState :: T.BattleSetting -> T.BattleState
initializeBattleState s = T.BattleState (initCards $ s ^. T.first) (initCards $ s ^. T.second) [] [] 0
    where maxHp' x = x ^. T.properties . T.maxHp
          maxMp' x = x ^. T.properties . T.maxMp
          initCards = map (\x -> T.CardState (maxHp' x) (maxMp' x))

isRunning :: BattleMachine Bool
isRunning = do
    (e, s) <- get
    return $ firstAlive s && secondAlive s && turnRemain e s
    where firstAlive x = sum (map cardAlive (x ^. T.first)) > 0
          secondAlive x = sum (map cardAlive (x ^. T.second)) > 0
          cardAlive :: T.CardState -> Int
          cardAlive c = if (c ^. T.hp) > 0 then 1 else 0
          turnRemain e s = case e ^. T.maxTurn of
                              Nothing -> True
                              Just n -> s ^. T.turn < n

toBattleMachine :: T.BattleTurn a -> BattleMachine a
toBattleMachine x = do
    (setting', state') <- get
    let (r, s, l) = runRWS (runErrorT x) setting' state'
    case r of
        Left m -> outputError m
        Right m -> do
            put (setting', s)
            tell [T.BattleLog s l []]
            return m

sortBattleCommands :: [T.BattleCommand] -> T.BattleTurn [T.BattleCommand]
sortBattleCommands cs = do
    speedFactors <- forM cs currentSpeed
    let zipped = zip speedFactors cs
    return $ map snd (sortWith fst zipped)
    where currentSpeed (T.BattleCommand p c a _) = currentProperties p c >>= \q -> return (a, q ^. T.speed)

toBattleCommand :: T.Player -> T.PlayerCommand -> BattleMachine T.BattleCommand
toBattleCommand p (T.PlayerCommand c s t)  = do
    (setting', state') <- get
    card' <- fromJust $ setting' ^? T.playerAccessor p . ix c
    T.Skill a tc <- fromJust $ card' ^? T.skills . ix s
    let targets = enumerateTargets setting' state' p c (targetable tc)
    target' <- fromJust $ targets ^? ix t
    return $ T.BattleCommand p c a target'
    where fromJust (Just a) = return a
          fromJust Nothing = outputError "in toBattleCommand. fromJust: Nothing"

battleTurn :: BattleMachine ()
battleTurn = do
    t <- liftM (^. _2 . T.turn) get
    xs <- enumerateCommandChoice T.FirstPlayer >>= inputPlayerCommands t T.FirstPlayer
    ys <- enumerateCommandChoice T.SecondPlayer >>= inputPlayerCommands t T.SecondPlayer
    xs' <- mapM (toBattleCommand T.FirstPlayer) xs
    ys' <- mapM (toBattleCommand T.SecondPlayer) ys
    toBattleMachine $ execTurn (xs' ++ ys')
    consumeTurn
    cutoffHpMp

execTurn :: [T.BattleCommand] -> T.BattleTurn ()
execTurn cs = do
    sorted <- sortBattleCommands cs
    forM_ sorted execCommand
    s <- get
    put $ s & T.oneTurnEffects .~ []

execCommand :: T.BattleCommand -> T.BattleTurn ()
execCommand (T.BattleCommand p c a t) = do
    state' <- get
    card' <- getCardState state'
    if canPerform card' a
        then execAction p c a t
        else tell [T.BattleCommandLog (T.BattleCommand p c a t) [T.Underqualified]]
    where getCardState s = case s ^? T.playerAccessor p . ix c of
                                Nothing -> throwError "in execCommand."
                                Just x -> return x

cutoffHpMp :: BattleMachine ()
cutoffHpMp = do
    (setting', state') <- get
    firstProp <- getProperties setting' state' T.FirstPlayer
    secondProp <- getProperties setting' state' T.SecondPlayer
    let firstCutoff = zipWith cutoff (state' ^. T.first) firstProp
    let secondCutoff = zipWith cutoff (state' ^. T.second) secondProp
    put (setting', (state' & T.first .~ firstCutoff) & T.second .~ secondCutoff)
    where getProperties e s p = case rs of
                                     Nothing -> outputError "in cutoffHpMp."
                                     Just x -> return x
            where rs = forM cards (currentProperties' e s p)
                  cards = [0..(cardNum - 1)]
                  cardNum = length $ e ^. T.playerAccessor p
          cutoff s p = T.CardState (min (s ^. T.hp) (p ^. T.maxHp)) (min (s ^. T.mp) (p ^. T.maxMp))

consumeTurn :: BattleMachine ()
consumeTurn = do
    (e, s) <- get
    let consumed = map consume (s ^. T.effects)
    let filtered' = filter activeEffect consumed
    put (e, (s & T.effects .~ filtered') & T.turn %~ (+1))
        where consume (e, n) = (e, n - 1)
              activeEffect e = e ^. _2 > 0

enumerateCommandChoice :: T.Player -> BattleMachine [T.CommandChoice]
enumerateCommandChoice p = do
    (setting', state') <- get
    let cards = setting' ^. T.playerAccessor p
    let cardStates = state' ^. T.playerAccessor p
    let cardNum = length cards
    let withIndex = zip3 [0..(cardNum - 1)] cards cardStates
    mapM applyCard . filter active $ withIndex
    where active (_, _, x) = x ^. T.hp > 0
          applyCard (i, x, y) = enumerateActionChoice p i x y >>= \ac -> return $ T.CommandChoice i ac

enumerateActionChoice :: T.Player -> Int -> T.Card -> T.CardState -> BattleMachine [T.ActionChoice]
enumerateActionChoice p c q s = do
    (setting', state') <- get
    return $ actionChoices setting' state'
    where executables = filter (canPerform s . (^. T.action)) (q ^. T.skills)
          withIndex = zip [0..(length executables - 1)] executables
          actionChoices e a = map (apply e a) withIndex
          apply e x (i, T.Skill a t) = T.ActionChoice i a (enumerateTargets e x p c (targetable t)) 

