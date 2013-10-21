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
import Control.Lens hiding (Action)

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
loadSecondPlayerCards = createInput LoadSecondPlayerCards

inputFirstPlayerCommands :: [CommandChoice] -> BattleMachine [PlayerCommand]
inputFirstPlayerCommands cs = createInput (InputFirstPlayerCommands cs) >>= checkInputCommands cs

inputSecondPlayerCommands :: [CommandChoice] -> BattleMachine [PlayerCommand]
inputSecondPlayerCommands cs = createInput (InputSecondPlayerCommands cs) >>= checkInputCommands cs

outputBattleState :: BattleState -> BattleMachine ()
outputBattleState = createOutput OutputBattleState

outputMessage :: String -> BattleMachine ()
outputMessage = createOutput OutputMessage

checkInputCommands :: [CommandChoice] -> [PlayerCommand] -> BattleMachine [PlayerCommand]
checkInputCommands cs ps = do
    checkLength
    forM_ (zip cs ps) check
    return ps
    where checkLength = if length cs /= length ps
                           then throwError "in checkInputCommand. invalid size."
                           else forM (zip cs ps) check
          check (a, b) = if a ^. cardIndex /= b ^. cardIndex
                            then throwError "in checkInputCommand. invalid cardIndex."
                            else checkAction a b
          checkAction a b = case a ^? actions . ix (b ^. skillIndex) of
                                 Nothing -> throwError "in checkInputCommand. invalid skillIndex."
                                 Just x -> checkTarget (x ^. targets) (b ^. targetIndex)
          checkTarget a b = case a ^? ix b of
                                 Nothing -> throwError "in checkInputCommand. invalid targetIndex."
                                 Just x -> return ()

