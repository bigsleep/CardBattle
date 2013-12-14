{-# LANGUAGE TypeOperators #-}
module Battle.Target
    ( enumerateAllTargets
    , enumerateTargets 
    , enumerateAsCards
    , Targetable
    , targetable
    , targetableAlmighty
    , targetableNothing
    , targetableSelf
    , targetableOwn
    , targetableOpponent
    , targetableOne
    , targetableTeam
    , targetableAll
    , targetableAlive
    , targetableDead
    ) where

import Control.Applicative((<$>), liftA2)
import Control.Monad.Reader(Reader, runReader)
import Control.Monad.Reader.Class(ask)
import Control.Lens ((^.), (^?), ix)
import Battle.Types

-- ターゲット全列挙
enumerateAllTargets :: BattleSetting -> [Target]
enumerateAllTargets e =
    f ++ s ++ [TargetTeam FirstPlayer, TargetTeam SecondPlayer, TargetAll]
    where f = map (TargetCard FirstPlayer) [0..(length (e ^. first) - 1)]
          s = map (TargetCard SecondPlayer) [0..(length (e ^. second) - 1)]

-- ターゲット列挙
enumerateTargets :: BattleSetting -> BattleState -> Player -> Int -> Targetable -> [Target]
enumerateTargets e s p c x = filtered
        where filtered = filter f (enumerateAllTargets e)
              f = curry (runReader x) (e, s, p, c)

-- 対象をカードとして列挙
enumerateAsCards :: BattleSetting -> Target -> [(Player, Int)]
enumerateAsCards _ (TargetCard p c) = [(p, c)]
enumerateAsCards e (TargetTeam p) = map (\c -> (p, c)) [0..length']
    where length' = length $ e ^. playerAccessor p
enumerateAsCards e TargetAll = enumerate FirstPlayer ++ enumerate SecondPlayer
    where enumerate p = enumerateAsCards e (TargetTeam p)

-- targetable
type Targetable = Reader ((BattleSetting, BattleState, Player, Int), Target) Bool

-- 全て
targetableAlmighty :: Targetable
targetableAlmighty = return True

-- 空
targetableNothing :: Targetable
targetableNothing = return False

-- カード位置同じ
targetableSamePosition :: Targetable
targetableSamePosition = ask >>= f
    where f ((_, _, _, c), TargetCard _ d) = return (c == d)
          f _ = return False

-- 敵
targetableOpponent :: Targetable
targetableOpponent = ask >>= f
    where f ((_, _, p, _), TargetCard q _) = return (p /= q)
          f ((_, _, p, _), TargetTeam q) = return (p /= q)
          f _ = return False

-- 味方
targetableOwn :: Targetable
targetableOwn = not <$> targetableOpponent

-- 単体
targetableOne :: Targetable
targetableOne = ask >>= f
    where f (_, TargetCard _ _) = return True
          f _ = return False

-- チーム
targetableTeam :: Targetable
targetableTeam = ask >>= f
    where f (_, TargetTeam _) = return True
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

-- 生死
targetableAlive, targetableDead :: Targetable
targetableAlive = ask >>= f
    where f ((_, s, p, c), _) = case s ^? playerAccessor p . ix c . hp of
                                     Nothing -> return False
                                     Just hp' -> return (hp' > 0)
targetableDead = ask >>= f
    where f ((_, s, p, c), _) = case s ^? playerAccessor p . ix c . hp of
                                     Nothing -> return False
                                     Just hp' -> return (hp' <= 0)

targetable :: TargetCapacity -> Targetable
targetable TargetCapacityOne = targetableOne
targetable TargetCapacityTeam = targetableTeam
targetable TargetCapacityAll = targetableAll
targetable TargetCapacityAlmighty = targetableAlmighty
targetable TargetCapacityOwn = targetableOwn
targetable TargetCapacityOpponent = targetableOpponent
targetable TargetCapacitySelf = targetableSelf
targetable TargetCapacityAlive = targetableAlive
targetable TargetCapacityDead = targetableDead
targetable (TargetCapacityMixAnd a) = foldl comb (return True) (map targetable a)
    where comb = liftA2 (&&)
targetable (TargetCapacityMixOr a) = foldl comb (return False) (map targetable a)
    where comb = liftA2 (||)
