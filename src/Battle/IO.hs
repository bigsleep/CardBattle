{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Battle.IO
    ( BattleIO(..)
    , BattleMachine
    , createInput
    , createOutput
    , loadSetting
    , inputPlayerCommands
    , outputBattleState
    , outputMessage
    , outputError
    , checkInputCommands
    ) where

import Control.Monad (forM_, when)
import Control.Monad.Error (MonadError, throwError, catchError)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Free (Free(Free, Pure))
import Control.Lens ((^.), (^?), ix)

import qualified Battle.Types as T

data BattleIO a =
    LoadSetting (T.BattleSetting -> a) |
    InputPlayerCommands Int T.Player [T.CommandChoice] ([T.PlayerCommand] -> a) |
    OutputBattleState T.BattleState a |
    OutputMessage String a |
    OutputError String

instance Functor BattleIO where
    fmap f (LoadSetting g) = LoadSetting (f . g)
    fmap f (InputPlayerCommands t p cs g) = InputPlayerCommands t p cs (f . g)
    fmap f (OutputBattleState s c) = OutputBattleState s (f c)
    fmap f (OutputMessage s c) = OutputMessage s (f c)
    fmap _ (OutputError s) = OutputError s

instance MonadError String (Free BattleIO) where
    throwError s = Free (OutputError s)
    catchError (Free (OutputError s)) f = f s
    catchError a _ = a

type BattleMachine = RWST () [T.BattleLog] (T.BattleSetting, T.BattleState) (Free BattleIO)

createInput :: ((a -> Free BattleIO a) -> BattleIO (Free BattleIO a)) -> BattleMachine a
createInput f = lift . Free . f $ \x -> Pure x

createOutput :: (a -> Free BattleIO () -> BattleIO (Free BattleIO ())) -> a -> BattleMachine ()
createOutput f x = lift . Free $ f x (Pure ())

loadSetting :: BattleMachine T.BattleSetting
loadSetting = createInput LoadSetting

inputPlayerCommands :: Int -> T.Player -> [T.CommandChoice] -> BattleMachine [T.PlayerCommand]
inputPlayerCommands t p cs = createInput (InputPlayerCommands t p cs) >>= checkInputCommands cs

outputBattleState :: T.BattleState -> BattleMachine ()
outputBattleState = createOutput OutputBattleState

outputMessage :: String -> BattleMachine ()
outputMessage = createOutput OutputMessage

outputError :: String -> BattleMachine a
outputError = lift . Free . OutputError

checkInputCommands :: [T.CommandChoice] -> [T.PlayerCommand] -> BattleMachine [T.PlayerCommand]
checkInputCommands cs ps = do
     checkLength
     forM_ (zip cs ps) check
     return ps
     where checkLength = when (length cs /= length ps) (outputError $ "in checkInputCommand. invalid size. " ++ show cs ++ show ps)
           check (a, b) = if a ^. T.cardIndex /= b ^. T.cardIndex
                             then outputError $ "in checkInputCommand. invalid cardIndex." ++ show cs ++ show ps
                             else checkAction a b
           checkAction a b = case a ^? T.actions . ix (b ^. T.skillIndex) of
                                  Nothing -> outputError $ "in checkInputCommand. invalid skillIndex." ++ show cs ++ show ps
                                  Just x -> checkTarget (x ^. T.targets) (b ^. T.targetIndex)
           checkTarget a b = case a ^? ix b of
                                  Nothing -> outputError $ "in checkInputCommand. invalid targetIndex." ++ show cs ++ show ps
                                  Just _ -> return ()
