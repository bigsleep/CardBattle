module Battle.TargetSpec where

import Test.Hspec
import Control.Lens hiding (Action)
import Battle.Types
import Battle.Target
import Battle.TestUtil

spec :: Spec
spec = do
    describe "Target" $ do
    it "enumerateAllTragets" $ do
        enumerateAllTargets defaultSetting `shouldBe` ans
        where ans = first' ++ second' ++ rest
              first' = tcards FirstPlayer defaultSetting
              second' = tcards SecondPlayer defaultSetting
              rest = [TargetTeam FirstPlayer, TargetTeam SecondPlayer, TargetAll]
              tcards p s = map (\c -> TargetCard p c) [0..(cardNum p s - 1)]
              cardNum p s = length (s ^. playerAccessor p)
