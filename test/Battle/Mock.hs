{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Battle.Mock
    ( BattleScenario(..)
    , MockIO
    , runMockIO
    , runBattleOnMockIO
    )
    where

import Control.Monad.Reader (Reader)
import Control.Monad.Error (runErrorT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.RWS (RWS, runRWST)
import Control.Monad.Free (Free(Free, Pure))
import Control.Lens hiding (Action, setting)

import Battle.Types
import Battle.Battle
import Battle.IO

data BattleScenario = BattleScenario {
    _battlemockSetting :: BattleSetting,
    _battlemockFirst :: [[PlayerCommand]],
    _battlemockSecond :: [[PlayerCommand]]
} deriving (Show, Eq)
$(makeFields ''BattleScenario)

type MockIO = Reader BattleScenario

runMockIO :: Free BattleIO a -> MockIO a

runMockIO (Pure a) = return a
runMockIO (Free (LoadSetting f)) = ask >>= runMockIO . f . (^. setting)
runMockIO (Free (InputPlayerCommands t p cs f)) = do
    scenario <- ask
    let c = (scenario ^. playerAccessor p) !! t
    runMockIO (f c)
runMockIO (Free (OutputBattleState s c)) = runMockIO c
runMockIO (Free (OutputMessage s c)) = runMockIO c

runBattleOnMockIO :: BattleMachine a -> MockIO (Either String a, (BattleSetting, BattleState), [BattleLog])
runBattleOnMockIO m = do
    e <- ask
    let setting' = e ^. setting
    let a = runRWST (runErrorT m) () (setting', initializeBattleState setting')
    runMockIO a
