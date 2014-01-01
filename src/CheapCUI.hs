module CheapCUI (runBattleOnCUI) where

import System.Random (randomRIO)
import Control.Monad (forM, forM_)
import Control.Monad.Trans (lift)
import Control.Monad.Free (Free(..))
import Control.Monad.State (get, put, StateT, runStateT)
import Control.Monad.Trans.RWS (runRWST)
import Control.Exception (throwIO)
import Control.Lens ((^.), (^?), ix)
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as BS (readFile)
import qualified Data.Aeson as DA (decode)

import qualified Battle.Types as T
import Battle.Battle (battle)
import Battle.IO

type CUI = StateT T.BattleSetting IO

runCUI :: Free BattleIO a -> CUI ()

runCUI (Pure _) = return ()

runCUI (Free (LoadSetting f)) = do
    s <- lift $ BS.readFile "battleSetting.json"
    setting <- lift . fromJust "設定ファイルのデコードに失敗しました。" . DA.decode $ s
    put setting
    runCUI (f setting)

runCUI (Free (InputPlayerCommands _ T.FirstPlayer cs f)) = do
    e <- get
    let cards = e ^. T.first
    lift . putStrLn $ "コマンド入力:"
    commands <- lift $ forM cs (inputCommandForCard cards)
    runCUI (f commands)

runCUI (Free (InputPlayerCommands _ T.SecondPlayer cs f)) = do
    commands <- lift $ forM cs randomCommandForCard
    lift . putStrLn $ "敵: " ++ show commands
    runCUI (f commands)

runCUI (Free (OutputBattleState s c)) = do
    lift . putStrLn $ "state: " ++ show s ++ "\n"
    runCUI c

runCUI (Free (OutputTurnResult log c)) = do
    runCUI c

runCUI (Free (OutputBattleResult s c)) = do
    runCUI c

runCUI (Free (OutputMessage m c)) = (lift . putStrLn $ m) >> runCUI c

runCUI (Free (OutputError s)) = lift . putStrLn $ ("エラー: " ++ s)

runBattleOnCUI :: IO ()
runBattleOnCUI = do
    _ <- runStateT a dummySetting
    return ()
    where dummySetting = T.BattleSetting [] [] Nothing
          dummyState = T.BattleState [] [] [] [] 0
          a = runCUI $ runRWST battle () (dummySetting, dummyState)

inputCommandForCard :: [T.Card] -> T.CommandChoice -> IO T.PlayerCommand
inputCommandForCard cards commandChoice = do
    card <- fromJust "out of range" $ cards ^? ix cardIndex
    outputCard card
    forM_ (zip [0..] actions) (outputActionChoice card)
    a <- readInput [0..(length actions - 1)]
    skill <- fromJust "out of range" $ actions ^? ix a
    let ts = skill ^. T.targets
    forM_ (zip [0..] ts) outputTargetChoice
    t <- readInput [0..(length ts - 1)]
    return $ T.PlayerCommand cardIndex (skill ^. T.skillIndex) t
    where cardIndex = commandChoice ^. T.cardIndex
          actions = commandChoice ^. T.actions

randomCommandForCard :: T.CommandChoice -> IO T.PlayerCommand
randomCommandForCard (T.CommandChoice cardIndex actionChoices) = do
    a <- randomRIO (0, length actionChoices - 1)
    ac <- fromJust "out of range" $ actionChoices ^? ix a
    let targets = ac ^. T.targets
    t <- randomRIO (0, length targets - 1)
    return $ T.PlayerCommand cardIndex (ac ^. T.skillIndex) t

outputCard :: T.Card -> IO ()
outputCard card = putStrLn $ card ^. T.name

outputActionChoice :: T.Card -> (Int, T.ActionChoice) -> IO ()
outputActionChoice card (i, actionChoice) = do
    skill <- fromJust "out of range" $ card ^? T.skills . ix skillIndex
    printf "%d: %s\n" i (skill ^. T.name)
    where skillIndex = actionChoice ^. T.skillIndex

outputTargetChoice :: (Int, T.Target) -> IO ()
outputTargetChoice (i, t) = putStrLn $ show i ++ ": " ++ show t

readInput :: (Eq a, Read a) => [a] -> IO a
readInput xs = do
    s <- getLine
    i <- cast s
    if i `elem` xs
        then return i
        else putStrLn "もう一度入力して下さい" >> readInput xs
    where cast s = case readMaybe s of
                        Just i -> return i
                        Nothing -> putStrLn "もう一度入力して下さい" >> readInput xs

fromJust :: String -> Maybe a -> IO a
fromJust _ (Just x) = return x
fromJust message Nothing = throwIO . userError $ message
