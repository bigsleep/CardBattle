module Battle.IO where

import Data.Map
import Control.Monad.State(StateT)
import Control.Monad.State.Class(get, put)
import Control.Monad.Reader.Class(ask)
import Control.Monad.Trans
import Control.Monad.Free(Free(Free, Pure))
import Battle.Types

data BattleIO a =
    LoadSetting (Maybe Int -> a) |
    LoadFirstPlayerCards (Map CardPosition Card -> a) |
    LoadSecondPlayerCards (Map CardPosition Card -> a) |
    InputFirstPlayerCommand (PlayerCommand -> a) |
    InputSecondPlayerCommand (PlayerCommand -> a) |
    OutputBattleState BattleState a |
    OutputMessage String a

instance Functor BattleIO where
    fmap f (LoadSetting g) = LoadSetting (f . g)
    fmap f (LoadFirstPlayerCards g) = LoadFirstPlayerCards (f . g)
    fmap f (LoadSecondPlayerCards g) = LoadSecondPlayerCards (f . g)
    fmap f (InputFirstPlayerCommand g) = InputFirstPlayerCommand (f . g)
    fmap f (InputSecondPlayerCommand g) = InputSecondPlayerCommand (f . g)
    fmap f (OutputBattleState s c) = OutputBattleState s (f c)
    fmap f (OutputMessage s c) = OutputMessage s (f c)

type BattleMachine = StateT (BattleSetting, BattleState) (Free BattleIO)

createInput :: ((a -> Free BattleIO a) -> BattleIO (Free BattleIO a)) -> BattleMachine a
createInput f = lift . Free . f $ \x -> Pure x

createOutput :: (a -> Free BattleIO () -> BattleIO (Free BattleIO ())) -> a -> BattleMachine ()
createOutput f x = lift . Free $ f x (Pure ())
