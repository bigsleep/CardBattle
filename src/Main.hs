import CheapCUI (runBattleOnCUI)

main :: IO ()
main = putStrLn "* バトル開始" >> runBattleOnCUI >> putStrLn "* バトル終了"
