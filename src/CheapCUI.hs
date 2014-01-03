module CheapCUI (runBattleOnCUI) where

import System.Random (randomRIO)
import System.IO (stdout, hFlush)
import Control.Monad (forM, forM_, when)
import Control.Monad.Trans (lift)
import Control.Monad.Free (Free(..))
import Control.Monad.State (get, put, StateT, runStateT)
import Control.Monad.Trans.RWS (runRWST)
import Control.Exception (throwIO)
import Control.Lens ((^.), (^?), ix)
import Control.Concurrent (threadDelay)
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
    setting <- lift . fromJust "設定ファイルのデコードに失敗しました" . DA.decode $ s
    put setting
    runCUI (f setting)

runCUI (Free (InputPlayerCommands t T.FirstPlayer cs f)) = do
    e <- get
    lift $ putStr "\n"
    lift $ printf "* %dターン目\n\n" (t + 1)
    lift $ putStrLn "* コマンド入力:"
    commands <- lift $ forM cs (inputCommandForCard e T.FirstPlayer)
    runCUI (f commands)

runCUI (Free (InputPlayerCommands _ T.SecondPlayer cs f)) = do
    commands <- lift $ forM cs randomCommandForCard
    runCUI (f commands)

runCUI (Free (OutputBattleState s c)) = do
    e <- get
    lift $ delay >> putStrLn "\n* カードの状態"
    lift $ putStrLn "あなたのチーム:"
    lift $ forM_ (zip3 [0..] (e ^. T.first) (s ^. T.first)) outputCardState
    lift $ putStrLn "敵チーム:"
    lift $ forM_ (zip3 [0..] (e ^. T.second) (s ^. T.second)) outputCardState
    runCUI c

runCUI (Free (OutputTurnResult l c)) = do
    setting <- get
    lift $ forM_ commandLogs (outputCommandLog setting)
--    forM_ effectExpirations outputEffectExpiration
    runCUI c
    where (T.BattleLog state commandLogs effectExpirations) = l

runCUI (Free (OutputBattleResult s c)) = do
    runCUI (Free (OutputBattleState s (Pure ())))
    lift $ putStr "\n"
    lift $ printf "* バトル結果: %s\n\n" battleResult
    runCUI c
    where dead card = card ^. T.hp <= 0
          allDead cards = all dead cards
          isFirstAllDead = allDead (s ^. T.first)
          isSecondAllDead = allDead (s ^. T.second)
          hpSum = sum . map (^. T.hp)
          result True True = "引き分け"
          result False True = "あなたの勝ち"
          result True False = "あなたの負け"
          result _ _ = desicion (hpSum (s ^. T.first)) (hpSum (s ^. T.second))
          desicion a b | a == b = "引き分け"
          desicion a b | a > b = "あなたの勝ち"
          desicion _ _ = "あなたの負け"
          battleResult = result isFirstAllDead isSecondAllDead

runCUI (Free (OutputMessage m c)) = (lift . putStrLn $ m) >> runCUI c

runCUI (Free (OutputError s)) = lift . putStrLn $ ("エラー: " ++ s)

runBattleOnCUI :: IO ()
runBattleOnCUI = do
    _ <- runStateT a dummySetting
    return ()
    where dummySetting = T.BattleSetting [] [] Nothing
          dummyState = T.BattleState [] [] [] [] 0
          a = runCUI $ runRWST battle () (dummySetting, dummyState)

inputCommandForCard :: T.BattleSetting -> T.Player -> T.CommandChoice -> IO T.PlayerCommand
inputCommandForCard setting player commandChoice = do
    card <- fromJust "out of range" $ cards ^? ix cardIndex
    delay
    printf "%sのカード%d [%s] の行動を%sの数値で入力して下さい\n" (toS T.FirstPlayer) cardIndex (toS card) (show [0..(length actions - 1)])
    forM_ (zip [0..] actions) (outputActionChoice card)
    a <- readInput [0..(length actions - 1)]
    skill <- fromJust "out of range" $ actions ^? ix a
    let ts = skill ^. T.targets
    printf "ターゲットを%sの数値で入力して下さい\n" (show [0..(length ts - 1)])
    forM_ (zip [0..] ts) (outputTargetChoice setting)
    t <- readInput [0..(length ts - 1)]
    putStr "\n"
    return $ T.PlayerCommand cardIndex (skill ^. T.skillIndex) t
    where cards = setting ^. T.playerAccessor player
          cardIndex = commandChoice ^. T.cardIndex
          actions = commandChoice ^. T.actions

randomCommandForCard :: T.CommandChoice -> IO T.PlayerCommand
randomCommandForCard (T.CommandChoice cardIndex actionChoices) = do
    a <- randomRIO (0, length actionChoices - 1)
    ac <- fromJust "out of range" $ actionChoices ^? ix a
    let targets = ac ^. T.targets
    t <- randomRIO (0, length targets - 1)
    return $ T.PlayerCommand cardIndex (ac ^. T.skillIndex) t

outputActionChoice :: T.Card -> (Int, T.ActionChoice) -> IO ()
outputActionChoice card (i, actionChoice) = do
    skill <- fromJust "out of range" $ card ^? T.skills . ix skillIndex
    printf "%d: %s\n" i (skill ^. T.name)
    where skillIndex = actionChoice ^. T.skillIndex

outputTargetChoice :: T.BattleSetting -> (Int, T.Target) -> IO ()
outputTargetChoice _ (i, T.TargetAll) = printf "%d: 全体\n" i
outputTargetChoice _ (i, T.TargetTeam T.FirstPlayer) = printf "%d: あなたのチーム\n" i
outputTargetChoice _ (i, T.TargetTeam T.SecondPlayer) = printf "%d: 敵チーム\n" i
outputTargetChoice setting (i, T.TargetCard p c) = do
    card <- fromJust "out of range" $ setting ^? T.playerAccessor p . ix c
    printf "%d: %sのカード%d [%s]\n" i (toS p) c (toS card)

outputCommandLog :: T.BattleSetting -> T.BattleCommandLog -> IO ()
outputCommandLog setting (T.BattleCommandLog command actionResults) = do
    card <- fromJust "out of range" $ setting ^? T.playerAccessor player . ix cardIndex
    skill <- fromJust "out of range" $ card ^? T.skills . ix skillIndex
    delay >> printf "%sのカード%d [%s] の行動 %s\n" (toS player) cardIndex (toS card) (toS skill)
    forM_ actionResults (\x -> delay >> outputActionResult setting x)
    where player = command ^. T.player
          cardIndex = command ^. T.cardIndex
          skillIndex = command ^. T.skillIndex
          target = command ^. T.target

outputActionResult :: T.BattleSetting -> T.ActionResult -> IO ()

outputActionResult _ (T.Consume (p, c) s) = do
    when (hp /= 0) (printf "HPを%d消費\n" hp)
    when (mp /= 0) (printf "MPを%d消費\n" mp)
    where hp = s ^. T.hp
          mp = s ^. T.mp

outputActionResult setting (T.StateChange (p, c) s) = do
    card <- fromJust "out of range" $ setting ^? T.playerAccessor p . ix c
    when (hp == 0 && mp == 0) (printf "%sのカード%d [%s] には効果がなかった\n" (toS p) c (toS card))
    when (hp /= 0) (printf "%sのカード%d [%s] のHPが%d%s\n" (toS p) c (toS card) (abs hp) (signToS hp))
    when (mp /= 0) (printf "%sのカード%d [%s] のMPが%d%s\n" (toS p) c (toS card) (abs mp) (signToS mp))
    where hp = s ^. T.hp
          mp = s ^. T.mp
          signToS a | a >= 0 = "増加した"
          signToS _ = "減少した"

outputActionResult setting (T.PropertyChange (p, c) property f) = do
    card <- fromJust "out of range" $ setting ^? T.playerAccessor p . ix c
    printf "%sのカード%d [%s] の%sが%d%%%s\n" (toS p) c (toS card) (toS property) (abs f) (signToS f)
    where signToS a | a >= 0 = "増加した"
          signToS _ = "減少した"

outputActionResult setting (T.Death (p, c)) = do
    card <- fromJust "out of range" $ setting ^? T.playerAccessor p . ix c
    printf "%sのカード%d [%s] は死んだ\n" (toS p) c (toS card)

outputActionResult _ T.FailureBecauseDeath =
    putStrLn "死んでいるため失敗した"

outputActionResult _ T.Underqualified =
    putStrLn "できなかった"

outputActionResult _ T.ActionFailure =
    putStrLn "失敗した"

outputCardState :: (Int, T.Card, T.CardState) -> IO ()
outputCardState (i, c, s) = do
    printf "カード%d [%s]\t| HP: %3d  MP: %3d |\n" i (toS c) (s ^. T.hp) (s ^. T.mp)

readInput :: (Eq a, Read a) => [a] -> IO a
readInput xs = do
    putStr "> " >> hFlush stdout
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

delay :: IO ()
delay = threadDelay 400000

class ToS a where
    toS :: a -> String

instance ToS T.Player where
    toS T.FirstPlayer = "あなた"
    toS T.SecondPlayer = "敵"

instance ToS T.Card where
    toS = (^. T.name)

instance ToS T.Skill where
    toS = (^. T.name)

instance ToS T.PropertyTag where
    toS T.MaxHpTag = "最大HP"
    toS T.MaxMpTag = "最大MP"
    toS T.AttackTag = "攻撃力"
    toS T.DefenseTag = "防御力"
    toS T.SpeedTag = "素早さ"
    toS T.MagicTag = "魔法"
