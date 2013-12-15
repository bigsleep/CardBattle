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

import Control.Monad.Identity (Identity)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.RWS (runRWST)
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

runMockIO :: Free BattleIO a -> MockIO (Either String a)

runMockIO (Pure a) = return (Right a)
runMockIO (Free (LoadSetting f)) = ask >>= runMockIO . f . (^. setting)
runMockIO (Free (InputPlayerCommands t p _ f)) = do
    scenario <- ask
    let c = (scenario ^. playerAccessor p) !! t
    runMockIO (f c)
runMockIO (Free (OutputBattleState _ c)) = runMockIO c
runMockIO (Free (OutputMessage _ c)) = runMockIO c
runMockIO (Free (OutputError s)) = return . Left $ s

runBattleOnMockIO :: ReaderT BattleScenario Identity (Either String ((), (BattleSetting, BattleState), [BattleLog]))
runBattleOnMockIO = do
    e <- ask
    let setting' = e ^. setting
    let a = runRWST battle () (setting', initializeBattleState setting')
    runMockIO a
