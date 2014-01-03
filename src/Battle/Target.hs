{-# LANGUAGE TypeOperators #-}
module Battle.Target
    ( enumerateAllTargets
    , enumerateTargets
    , enumerateAsCards
    , targetable
    ) where

import Control.Applicative((<$>), liftA2)
import Control.Monad.Reader(Reader, runReader)
import Control.Monad.Reader.Class(ask)
import Control.Lens ((^.), (^?), ix)
import Prelude hiding (all)

import qualified Battle.Types as T

-- ターゲット全列挙
enumerateAllTargets :: T.BattleSetting -> [T.Target]
enumerateAllTargets e =
    f ++ s ++ [T.TargetTeam T.FirstPlayer, T.TargetTeam T.SecondPlayer, T.TargetAll]
    where f = map (T.TargetCard T.FirstPlayer) [0..(length (e ^. T.first) - 1)]
          s = map (T.TargetCard T.SecondPlayer) [0..(length (e ^. T.second) - 1)]

-- ターゲット列挙
enumerateTargets :: T.BattleSetting -> T.BattleState -> T.Player -> Int -> Targetable -> [T.Target]
enumerateTargets e s p c x = filtered
        where filtered = filter f (enumerateAllTargets e)
              f = curry (runReader x) (e, s, p, c)

-- 対象をカードとして列挙
enumerateAsCards :: T.BattleSetting -> T.Target -> [(T.Player, Int)]
enumerateAsCards _ (T.TargetCard p c) = [(p, c)]
enumerateAsCards e (T.TargetTeam p) = map (\c -> (p, c)) [0..(length' - 1)]
    where length' = length $ e ^. T.playerAccessor p
enumerateAsCards e T.TargetAll = enumerate T.FirstPlayer ++ enumerate T.SecondPlayer
    where enumerate p = enumerateAsCards e (T.TargetTeam p)

-- targetable
type Targetable = Reader ((T.BattleSetting, T.BattleState, T.Player, Int), T.Target) Bool

samePosition :: Targetable
samePosition = ask >>= f
    where f ((_, _, _, c), T.TargetCard _ d) = return (c == d)
          f _ = return False

opponent :: Targetable
opponent = ask >>= f
    where f ((_, _, p, _), T.TargetCard q _) = return (p /= q)
          f ((_, _, p, _), T.TargetTeam q) = return (p /= q)
          f _ = return False

own :: Targetable
own = not <$> opponent

one :: Targetable
one = ask >>= f
    where f (_, T.TargetCard _ _) = return True
          f _ = return False

team :: Targetable
team = ask >>= f
    where f (_, T.TargetTeam _) = return True
          f _ = return False

all :: Targetable
all = ask >>= f
    where f ((_, _, _, _), T.TargetAll) = return True
          f _ = return False

alive, dead :: Targetable
alive = ask >>= f
    where f (_, T.TargetAll) = return True
          f (_, T.TargetTeam _) = return True
          f ((_, s, _, _), T.TargetCard p c) = case s ^? T.playerAccessor p . ix c . T.hp of
                                                    Nothing -> return False
                                                    Just hp -> return (hp > 0)
dead = ask >>= f
    where f (_, T.TargetAll) = return True
          f (_, T.TargetTeam _) = return True
          f ((_, s, _, _), T.TargetCard p c) = case s ^? T.playerAccessor p . ix c . T.hp of
                                                    Nothing -> return False
                                                    Just hp -> return (hp <= 0)

(.&&) :: Targetable -> Targetable -> Targetable
(.&&) = liftA2 (&&)

targetable :: T.TargetCapacity -> Targetable
targetable T.TcAlmighty = return True
targetable T.TcOne = one
targetable T.TcAliveOne = alive .&& one
targetable T.TcDeadOne = dead .&& one
targetable T.TcTeam = team
targetable T.TcAll = all
targetable T.TcOpponentOne = opponent .&& one
targetable T.TcAliveOpponentOne = alive .&& opponent .&& one
targetable T.TcDeadOpponentOne = dead .&& opponent .&& one
targetable T.TcOpponentTeam = opponent .&& team
targetable T.TcOwnOne = opponent .&& one
targetable T.TcAliveOwnOne = alive .&& own .&& one
targetable T.TcDeadOwnOne = dead .&& own .&& one
targetable T.TcOwnTeam = own .&& team
targetable T.TcSelf = samePosition .&& own
