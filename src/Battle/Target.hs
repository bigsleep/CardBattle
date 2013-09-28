module Battle.Target where

import Control.Monad.State.Class(get, put)
import Control.Monad.Reader.Class(ask)
import Control.Lens hiding (Action)
import Battle.Types

onTarget :: Player -> Int -> Target -> Bool
onTarget _ _ TargetAll = True
onTarget q _ (TargetTeam p) = p == q
onTarget q y (TargetCard p x) = p == q && x == y

-- ターゲット全列挙
enumerateAllTargets :: BattleSetting -> [Target]
enumerateAllTargets e =
    f ++ s ++ [TargetTeam FirstPlayer, TargetTeam SecondPlayer, TargetAll]
    where f = map (\c -> TargetCard FirstPlayer c) [0..(length (e ^. firstCards) - 1)]
          s = map (\c -> TargetCard SecondPlayer c) [0..(length (e ^. secondCards) - 1)]

-- ターゲット列挙
enumerateTargets :: (Targetable a) => Player -> Int -> a -> BattleTurn [Target]
enumerateTargets p c x = do
    e <- ask
    s <- get
    return $ filter (canTarget e s p c x) (enumerateAllTargets e)

-- Targetable合成

-- 自分だけ
data TargetableSelf = TargetableSelf deriving (Show, Eq)

instance Targetable TargetableSelf where
    canTarget _ _ p c _ (TargetCard q d) = p == q && c == d
    canTarget _ _ _ _ _ _ = False

-- 敵一体
data TargetableOpponentOne = TargetableOpponentOne deriving (Show, Eq)

instance Targetable TargetableOpponentOne where
    canTarget _ _ p c _ (TargetCard q d) = p /= q
    canTarget _ _ _ _ _ _ = False
        
