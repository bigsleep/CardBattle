{-# LANGUAGE TypeOperators #-}
module Battle.Target where

import Control.Applicative((<*>), (<$>), liftA2)
import Control.Monad(filterM)
import Control.Monad.State.Class(get, put)
import Control.Monad.Reader(runReader, withReaderT)
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
enumerateTargets :: BattleSetting -> BattleState -> Player -> Int -> Targetable -> [Target]
enumerateTargets e s p c x = filtered
        where filtered = filter f (enumerateAllTargets e)
              f = (curry (runReader x)) (e, s, p, c)

-- 対象をカードとして列挙
enumerateAsCards :: BattleSetting -> Target -> [(Player, Int)]
enumerateAsCards e (TargetCard p c) = [(p, c)]
enumerateAsCards e (TargetTeam p) = map (\c -> (p, c)) [0..(length')]
    where length' = length $ e ^. (playerAccessor p)
enumerateAsCards e TargetAll = (enumerate FirstPlayer) ++ (enumerate SecondPlayer)
    where enumerate p = enumerateAsCards e (TargetTeam p)

-- targetable
targetable :: TargetCapacity -> Targetable
targetable TargetCapacityOne = targetableOne
targetable TargetCapacityTeam = targetableTeam
targetable TargetCapacityAll = targetableAll
targetable TargetCapacityAlmighty = targetableAlmighty
targetable TargetCapacityOwn = targetableOwn
targetable TargetCapacityOpponent = targetableOpponent
targetable TargetCapacitySelf = targetableSelf
targetable (TargetCapacityMixAnd a b) = targetable a `comb` targetable b
    where comb = liftA2 (&&)
targetable (TargetCapacityMixOr a b) = targetable a `comb` targetable b
    where comb = liftA2 (||)

-- 全て
targetableAlmighty :: Targetable
targetableAlmighty = return True


-- カード位置同じ
targetableSamePosition :: Targetable
targetableSamePosition = ask >>= f
    where f ((_, _, _, c), (TargetCard _ d)) = return (c == d)
          f _ = return False

-- 敵
targetableOpponent :: Targetable
targetableOpponent = ask >>= f
    where f ((_, _, p, _), (TargetCard q _)) = return (p /= q)
          f ((_, _, p, _), (TargetTeam q)) = return (p /= q)
          f _ = return False

-- 味方
targetableOwn :: Targetable
targetableOwn = not <$> targetableOpponent

-- 単体
targetableOne :: Targetable
targetableOne = ask >>= f
    where f ((_, _, _, _), (TargetCard _ _)) = return True
          f _ = return False

-- チーム
targetableTeam :: Targetable
targetableTeam = ask >>= f
    where f ((_, _, _, _), (TargetTeam _)) = return True
          f _ = return False

-- 全体
targetableAll :: Targetable
targetableAll = ask >>= f
    where f ((_, _, _, _), TargetAll) = return True
          f _ = return False

-- 自分
targetableSelf :: Targetable
targetableSelf = targetableSamePosition `comb` targetableOwn
    where comb = liftA2 (&&)
