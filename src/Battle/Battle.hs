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
import Control.Monad.Writer.Class (tell)
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
initializeBattleState s = BattleState (initCards $ s ^. first) (initCards $ s ^. second) [] [] (s ^. maxTurn)
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
    let (r, s, l) = runRWS (runErrorT x) setting' state'
    case r of
        Left m -> throwError m
        Right m -> do
            put (setting', s)
            tell [BattleLog s l []]
            return m

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
    xs <- (enumerateCommandChoice FirstPlayer) >>= inputFirstPlayerCommands
    ys <- (enumerateCommandChoice SecondPlayer) >>= inputSecondPlayerCommands
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
execCommand (BattleCommand p c a t) = execAction p c a t

cutoffHpMp :: BattleMachine ()
cutoffHpMp = do
    (setting', state') <- get
    firstProp <- getProperties setting' state' FirstPlayer
    secondProp <- getProperties setting' state' SecondPlayer
    let firstCutoff = map cutoff ((state' ^. first) `zip` firstProp)
    let secondCutoff = map cutoff ((state' ^. second) `zip` secondProp)
    put $ (setting', (state' & first .~ firstCutoff) & second .~ secondCutoff)
    where getProperties e s p = case rs of
                                     Nothing -> throwError "in cutoffHpMp."
                                     Just x -> return x
            where rs = forM cards (currentProperties' e s p)
                  cards = [0..(cardNum - 1)]
                  cardNum = length $ e ^. playerAccessor p
          cutoff (s, p) = CardState (min (s ^. hp) (p ^. maxHp)) (min (s ^. mp) (p ^. maxMp))

consumeTurn :: BattleMachine ()
consumeTurn = do
    (e, s) <- get
    let effs = s ^. effects
    let consumed = map consume effs
    let filtered = filter activeEffect consumed
    put $ (e, (s & effects .~ filtered) & remainingTurn %~ fmap (1-))
        where consume e = e & remaining %~ (fmap (1-))
              activeEffect e = case (e ^. remaining) of
                                    Nothing -> True
                                    (Just n) -> n > 0

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
    where executables = filter (canPerform s) (q ^. skills)
          withIndex = zip [0..(length executables - 1)] executables
          actionChoices e s = map (apply e s) withIndex
          apply e s (i, (Skill a t)) = ActionChoice i a (enumerateTargets e s p c (targetable t)) 

