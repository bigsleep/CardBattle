module Battle.IO where

import Data.Map
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.State(StateT)
import Control.Monad.State.Class(get, put)
import Control.Monad.Reader.Class(ask)
import Control.Monad.Trans
import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Free(Free(Free, Pure))
import Battle.Types

data BattleIO a =
    LoadSetting (Maybe Int -> a) |
    LoadFirstPlayerCards ([Card] -> a) |
    LoadSecondPlayerCards ([Card] -> a) |
    InputFirstPlayerCommands [CommandChoice] ([PlayerCommand] -> a) |
    InputSecondPlayerCommands [CommandChoice] ([PlayerCommand] -> a) |
    OutputBattleState BattleState a |
    OutputMessage String a

instance Functor BattleIO where
    fmap f (LoadSetting g) = LoadSetting (f . g)
    fmap f (LoadFirstPlayerCards g) = LoadFirstPlayerCards (f . g)
    fmap f (LoadSecondPlayerCards g) = LoadSecondPlayerCards (f . g)
    fmap f (InputFirstPlayerCommands cs g) = InputFirstPlayerCommands cs (f . g)
    fmap f (InputSecondPlayerCommands cs g) = InputSecondPlayerCommands cs (f . g)
    fmap f (OutputBattleState s c) = OutputBattleState s (f c)
    fmap f (OutputMessage s c) = OutputMessage s (f c)

type BattleMachine = ErrorT String (RWST () [BattleLog] (BattleSetting, BattleState) (Free BattleIO))

createInput :: ((a -> Free BattleIO a) -> BattleIO (Free BattleIO a)) -> BattleMachine a
createInput f = lift . lift . Free . f $ \x -> Pure x

createOutput :: (a -> Free BattleIO () -> BattleIO (Free BattleIO ())) -> a -> BattleMachine ()
createOutput f x = lift . lift . Free $ f x (Pure ())

loadSetting :: BattleMachine (Maybe Int)
loadSetting = createInput LoadSetting

loadFirstPlayerCards :: BattleMachine [Card]
loadFirstPlayerCards = createInput LoadFirstPlayerCards

loadSecondPlayerCards :: BattleMachine [Card]
loadSecondPlayerCards =createInput LoadSecondPlayerCards

inputFirstPlayerCommands :: [CommandChoice] -> BattleMachine [PlayerCommand]
inputFirstPlayerCommands cs = createInput $ InputFirstPlayerCommands cs

inputSecondPlayerCommands :: [CommandChoice] -> BattleMachine [PlayerCommand]
inputSecondPlayerCommands cs = createInput $ InputSecondPlayerCommands cs

outputBattleState :: BattleState -> BattleMachine ()
outputBattleState = createOutput OutputBattleState

outputMessage :: String -> BattleMachine ()
outputMessage = createOutput OutputMessage
