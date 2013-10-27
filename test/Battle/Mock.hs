module Battle.Mock

import Control.Monad.Reader
import Battle.Types
import Battle.IO

data BattleScenario = BattleScenario {
    _battlemockSetting :: BattleSetting,
    _battlemockFirst :: [PlayerCommand],
    _battlemockSecond :: [PlayerCommand]
} deriving (Show, Eq)

type MockIO = Reader BattleScenario Identity

runMockIO :: Free Battle.IO a -> MockIO a

runMockIO (Pure a) = return a
runMockIO (Free (LoadSetting f)) = ask >>= f . (^. setting)
runMockIO (Free (InputPlayerCommands t p cs f)) = do
    scenario <- ask
    let c = (scenario ^. playerAccessor p) !! t
    runMockIO (f c)
runMockIO (Free (OutputBattleState s c)) = runMock c
runMockIO (Free (OutputMessage s c)) = runMock c

runBattleOnMockIO :: BattleMachine a -> MockIO a
runBattleOnMockIO m = do
    e <- ask
    let setting' = e ^. setting'
    runMockIO $ runRWS (runErrorT m) () (setting', initializeBattleSetting setting')
