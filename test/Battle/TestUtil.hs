{-# LANGUAGE FlexibleInstances #-}
module Battle.TestUtil
    ( chooseTargetCard
    ) where

import Test.QuickCheck
import Control.Monad(replicateM)
import Control.Applicative
import Control.Lens hiding (Action, elements)

import Battle.Types

-- Arbitrary
instance Arbitrary Player where
    arbitrary = f <$> choose (True, False)
        where f True = FirstPlayer
              f False = SecondPlayer

instance Arbitrary PropertySet where
    arbitrary = do
        hp' <- choose (1, 100)
        mp' <- choose (1, 50)
        attack' <- choose (1, 20)
        defense' <- choose (1, 20)
        speed' <- choose (1, 20)
        magic' <- choose (1, 20)
        return $ PropertySet hp' mp' attack' defense' speed' magic'

instance Arbitrary Card where
    arbitrary = do
        p <- arbitrary
        n <- arbitrary
        return $ Card n p []

instance Arbitrary BattleSetting where
    arbitrary = do
        n <- choose (1, 3)
        m <- choose (1, 3)
        f <- replicateM n arbitrary
        s <- replicateM m arbitrary
        t <- choose (1, 10)
        return $ BattleSetting f s (Just t)

instance Arbitrary Target where
    arbitrary = oneof [genAll, genTeam, genCard]
        where genAll = return TargetAll
              genTeam = arbitrary >>= \p -> return $ TargetTeam p
              genCard = do {p <- arbitrary; c <- choose (0, 10); return $ TargetCard p c}

instance Arbitrary BattleEffect where
    arbitrary = do
        tp <- arbitrary
        tc <- choose (0, 10)
        p <- arbitrary
        f <- arbitrary
        let factor' = unitFactor & propertyAccessor p %~ (+f)
        return $ BattleEffect (tp, tc) factor'
        where unitFactor = PropertySet u u u u u u
              u = factorDenominator

instance Arbitrary PropertyTag where
    arbitrary = elements $ enumFrom MaxHpTag

--
chooseTargetCard :: BattleSetting -> Player -> Gen Int
chooseTargetCard e p = choose (0, cardNum - 1)
    where cardNum = length $ e ^. (playerAccessor p)
