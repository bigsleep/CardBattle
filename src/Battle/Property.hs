module Battle.Property
    ( currentProperties
    , currentProperties'
    , applyPropertyFactor
    , factorDenominator
    , toPropertyFactor
    ) where

import Control.Lens ((^.), (^?), (.~), (&), ix)
import Control.Monad.State.Class (get)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error (throwError)

import qualified Battle.Types as T

factorDenominator :: Int
factorDenominator = 100

currentProperties :: T.Player -> Int -> T.BattleTurn T.PropertySet
currentProperties p c = do
    e <- ask
    s <- get
    case currentProperties' e s p c of
         Nothing -> throwError "in currentProperties. list index out of range."
         Just x  -> return x

currentProperties' :: T.BattleSetting -> T.BattleState -> T.Player -> Int -> Maybe T.PropertySet
currentProperties' e s p c = fmap applyEffect card'
    where card' = e ^. T.playerAccessor p ^? ix c
          applyEffect (T.Card _ q _) = foldl applyPropertyFactor q (effectFactors (s ^. T.oneTurnEffects ++ map fst (s ^. T.effects)))
          effectFactors xs = map toPropertyFactor $ filter (onTarget p c) xs
          onTarget q d x = (q, d) == x ^. T.target

toPropertyFactor :: T.BattleEffect -> T.PropertySet
toPropertyFactor e = s & T.propertyAccessor tag .~ f
    where s = T.PropertySet u u u u u u
          f = e ^. T.factor
          u = factorDenominator
          tag = e ^. T.property

applyPropertyFactor :: T.PropertySet -> T.PropertySet -> T.PropertySet
applyPropertyFactor (T.PropertySet a1 b1 c1 d1 e1 f1) (T.PropertySet a2 b2 c2 d2 e2 f2) =
    T.PropertySet (a1 * a2 `div` factorDenominator)
                (b1 * b2 `div` factorDenominator)
                (c1 * c2 `div` factorDenominator)
                (d1 * d2 `div` factorDenominator)
                (e1 * e2 `div` factorDenominator)
                (f1 * f2 `div` factorDenominator)
